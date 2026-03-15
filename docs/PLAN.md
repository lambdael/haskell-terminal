
# Haskell Terminal Emulator

Haskellでターミナルエミュレータを作るプロジェクト。

レンダリングは、OpenGL。

最終的には、Haskellでシェーダ書いて、Xmonadみたいにリロードできるようにしたいのですよね。
なので、レンダリングをGpipeに移行する。

Gpipeはフォントレンダリングがないので作る。> gpipe-freefontプロジェクト

freetype2 バインディング + 手動テクスチャアトラス

## 進捗

### GPipe 移行 (2026-03-14)
- [x] gpipe-freetype でフォントレンダリング完成
- [x] `hsterm-gpipe` 実行ファイルを追加（GLUT/OpenGL/FTGL → GPipe/GPipe-GLFW/gpipe-freetype）
  - `Hsterm.GPipe.Main` — GPipe メインループ（GLFW ウィンドウ、キー入力、リサイズ）
  - `Hsterm.GPipe.Renderer` — テキスト・背景色・カーソルの頂点生成 + GPipe シェーダー
  - `Hsterm.GPipe.Terminal` — PTY 管理（OpenGL 非依存）
- [x] flake.nix に GPipe-Core, GPipe-GLFW, gpipe-freetype の依存を追加
- [x] `nix run` のデフォルトを `hsterm-gpipe` に変更
- [x] 旧 `hsterm`（GLUT版）を削除

### TODO
- [x] バッファの再利用（毎フレーム newBuffer しない）
- [x] 256色 / TrueColor 対応
- [x] スクロールバック（代替画面バッファ）
- [x] マウス 対応
- [x] マウスによる選択・コピー（ctrl+shift+c）・ペースト（ctrl+shift+v）
- [x] Xmonad 風ホットリロード対応

### Xmonad 風ホットリロード (Dyre)

Dyre ライブラリを使って、ユーザが Haskell で設定を書き、リコンパイル＋exec で反映するパターンを実装する。

#### 方針
- Dyre (`Config.Dyre`) でリコンパイル＋exec を管理
- ユーザ設定ファイル: `~/.config/haskell-terminal/config.hs`
- ユーザは `defaultConfig` を上書きして `main = hsterm myConfig` と書く
- PTY fd は exec 越しに引き継ぐ（`FD_CLOEXEC` を外す）
- Terminal 状態は一時ファイルにシリアライズして引き継ぐ
- GLFW ウィンドウ・GPU コンテキストは再作成（OpenGL コンテキストは exec で失われる）

#### TerminalConfig

```haskell
data TerminalConfig = TerminalConfig
  { -- 外観
    tcFontFamily    :: String
  , tcFontSize      :: Int
  , tcColorScheme   :: ColorScheme    -- 16スロット（色 or シェーダ）+ デフォルト前景/背景
  , tcCursorColor   :: V4 Float
  , tcCursorStyle   :: CursorStyle   -- Block | Underline | Bar

    -- ターミナル
  , tcInitialCols   :: Int
  , tcInitialRows   :: Int
  , tcScrollback    :: Int
  , tcShell         :: Maybe FilePath  -- Nothing = $SHELL

    -- 入力
  , tcKeyBindings   :: KeyBindings    -- Map KeyCombo (Terminal ())

    -- レンダリング
  , tcFrameDelay    :: Int            -- μs
  , tcShaders       :: ShaderConfig   -- カスタムシェーダ

    -- フック
  , tcStartupHook   :: Terminal ()
  , tcEventHook     :: Event -> Terminal () -> Terminal ()
  }
```

#### Terminal モナド

キーバインドやフックのアクション実行用。薄いラッパー。

```haskell
newtype Terminal a = Terminal (ReaderT TerminalEnv IO a)

data TerminalEnv = TerminalEnv
  { teConfig    :: TerminalConfig
  , teTermRef   :: IORef Terminal.Types.Terminal
  , tePtyHandle :: PtyHandle
  , teDirty     :: IORef Bool
  }
```

#### ColorScheme — 色スロットにシェーダを割り当てる

従来のターミナルでは 16 色スロットに静的な RGB 値を割り当てる。
このプロジェクトでは、各スロットに**色 or アニメーション付きシェーダ**を割り当てられるようにする。

```haskell
-- 各色スロットの定義: 静的な色か、動的シェーダ
data ColorSlot
  = SolidColor (V4 Float)                         -- 従来通りの静的色
  | AnimatedColor (Float -> V4 Float)              -- CPU 側: time -> color
  | ColorShader (ColorShaderDef)                   -- GPU 側: フラグメントシェーダ

-- GPU シェーダとして定義する場合
-- cellPos: セル座標 (col, row), time: 秒, cellSize: ピクセル
type ColorShaderDef = S F Float         -- time
                    -> V2 (S F Float)   -- cellPos (col, row)
                    -> V2 (S F Float)   -- fragCoord (セル内 0..1)
                    -> V4 (S F Float)   -- output RGBA

data ColorScheme = ColorScheme
  { csSlots      :: Array ANSIColor ColorSlot  -- 16スロット (8 normal + 8 bright)
  , csDefaultFg  :: ColorSlot
  , csDefaultBg  :: ColorSlot
  }
```

**使用例（ユーザの config.hs）**:
```haskell
myColors :: ColorScheme
myColors = defaultColorScheme
  { csSlots = defaultSlots
      // [ (Red,    ColorShader pulsingRed)      -- 赤セルが脈動する
         , (Blue,   ColorShader oceanWave)       -- 青セルが波のように動く
         , (Green,  SolidColor (hexColor "#00ff00"))  -- 緑は普通の色
         ]
  }

-- 脈動する赤
pulsingRed :: ColorShaderDef
pulsingRed time _cellPos _fragCoord =
  let pulse = (sin' (time * 2.0) + 1.0) * 0.5   -- 0..1 oscillation
      r = 0.5 + pulse * 0.5
  in  V4 r 0.1 0.1 1.0

-- 波のような青
oceanWave :: ColorShaderDef
oceanWave time cellPos fragCoord =
  let wave = sin' (cellPos ^. _x * 0.3 + time) * 0.15
      b = 0.4 + wave + fragCoord ^. _y * 0.2
  in  V4 0.05 (0.1 + wave * 0.5) b 1.0
```

**レンダリング戦略**:

| ColorSlot 種別 | 処理 |
|----------------|------|
| `SolidColor` | 従来通り、色テクスチャに書き込み。シェーダは単純サンプル |
| `AnimatedColor` | CPU 側で毎フレーム色を計算 → テクスチャに書き込み。dirty フラグ常時 on |
| `ColorShader` | フラグメントシェーダ内で分岐。セルのカラーインデックスに基づき対応する ShaderDef を呼ぶ |

`ColorShader` の GPU 実装方法:
- セルごとに色テクスチャに「カラーインデックス」(0〜17) を書き込む
- フラグメントシェーダ内で `ifThenElse'` チェーンでインデックスに応じたシェーダ関数を呼ぶ
- GPipe は Haskell の関数をそのまま GLSL にコンパイルするので、ユーザの ShaderDef がそのままフラグメントシェーダに埋め込まれる
- `SolidColor` スロットは定数として最適化される

**time uniform**:
- メインループで経過時間を `Uniform` バッファとしてシェーダに渡す
- `AnimatedColor` は CPU 側なのでフレームごとに `IORef` の時間を読む

#### ShaderConfig

ユーザが GPipe シェーダを書き換えられる（レイヤー単位）。

```haskell
data ShaderConfig = ShaderConfig
  { scBackgroundShader :: ShaderDef BgInput   -- 背景レイヤー全体
  , scTextShader       :: ShaderDef TextInput  -- テキストレイヤー全体
  , scCursorShader     :: ShaderDef CursorInput
  , scExtraLayers      :: [ShaderDef ExtraInput]  -- ポストエフェクト等
  }
```

`ShaderConfig` はレイヤー全体の制御、`ColorScheme` は色スロット単位の制御。両方組み合わせて使える。

#### Dyre 統合

```haskell
-- Hsterm/GPipe/EntryPoint.hs
import qualified Config.Dyre as Dyre

dyreParams :: Dyre.Params TerminalConfig
dyreParams = Dyre.newParams
  { Dyre.projectName = "haskell-terminal"
  , Dyre.realMain    = realMain     -- TerminalConfig -> IO ()
  , Dyre.showError   = showError    -- TerminalConfig -> String -> TerminalConfig
  , Dyre.configDir   = Just $ getXdgDirectory XdgConfig "haskell-terminal"
  , Dyre.cacheDir    = Just $ getXdgDirectory XdgCache  "haskell-terminal"
  }

main :: IO ()
main = Dyre.wrapMain dyreParams defaultConfig
```

#### リロードフロー

```
Ctrl+Shift+R （キーバインドで発火）
      │
      ▼
 saveState  ── PTY fd (FD_CLOEXEC 解除) + Terminal 状態シリアライズ
      │          → /tmp/haskell-terminal-state-XXXX (Binary/Cereal)
      ▼
 Dyre.recompile  ── GHC で ~/.config/haskell-terminal/config.hs を再コンパイル
      │               → ~/.cache/haskell-terminal/haskell-terminal-xxx
      ▼
 exec newBinary ["--resume", stateFile]
      │
      ▼
 新プロセス起動
      ├─ restoreState  ── 状態ファイル読み込み、PTY fd を Handle に復元
      ├─ GLFW ウィンドウ＋GPU コンテキスト再作成
      ├─ フォントアトラス再構築（新設定の tcFontFamily / tcFontSize を使用）
      └─ mainLoop 再開（端末内容・カーソル位置・スクロールバック完全復元）
```

#### 状態の引き継ぎ

| 項目 | 方法 |
|------|------|
| PTY fd | `FD_CLOEXEC` を外して exec → 新プロセスでコマンドライン引数から fd 番号を受け取る |
| Terminal バッファ | `Binary` でシリアライズ → 一時ファイル |
| カーソル位置・属性 | Terminal 状態に含まれる |
| スクロールバック | Terminal 状態に含まれる |
| ウィンドウサイズ | Terminal 状態に含まれる（GLFW 側は再作成） |
| GPU リソース | 引き継がない（再作成） |

#### 実装フェーズ

- [x] **Phase 1**: `TerminalConfig` + `defaultConfig` を定義。現在のハードコード値を集約
- [x] **Phase 2**: `Terminal` モナド導入。キーバインドをモナドアクションに移行
- [x] **Phase 3**: `Terminal.Types.Terminal` を `Serializable` にする
- [x] **Phase 4**: Dyre 統合 — `wrapMain` + リコンパイル + exec 置換
- [x] **Phase 5**: `ShaderConfig` でレイヤーシェーダカスタマイズ対応
- [x] **Phase 6**: `ColorScheme` に `ColorSlot` 導入 — 色スロットにシェーダを割当可能に
- [x] **Phase 7**: time uniform + アニメーション対応
- [x] **Phase 8**: サンプル `config.hs` を用意（アニメーションカラースキームのデモ含む）

#### 注意事項
- Dyre が足りない場合（PTY fd 引き継ぎのカスタム exec 等）は自前実装にフォールバック
- NixOS 環境では GHC の発見に注意（`dyre` の `ghcOpts` か、Nix で GHC を PATH に入れる必要あり）

### あとでやるかも
- [ ] ウィンドウリサイズ時のシェーダー再コンパイルを最適化、ウィンドウサイズを Uniform バッファで渡すようにする




## 各種プロジェクト
- gpipe-freetype: Gpipe フォントレンダリング
- GPipe-GLFW: Gpipe　GLFWバインディング、メンテナンスされてなかったのをメンテナンスした
- GPipe-Core: Gpipeの元、メンテナンスされてなかったのをメンテナンスした
- gpipe-shader: Gpipeのシェーダトイ相当のデモ


###　GpipeのOpenGLのバージョンを上げるべきか。
- 4.2: glTexStorage2D (Immutable texture)		アトラスの事前割り当てに便利だがなくても可
- 4.3: Compute Shader	グリフのSDF生成を GPU でやるなら。初期段階では不要
- 4.4(ARB): Bindless Texture		大量グリフの描画効率。初期段階では不要

後日対応する。



