# Haskell Terminal Emulator

Haskell で書かれた VT100/ANSI ターミナルエミュレータ。ライブラリとGUIアプリケーションの2つの側面を持つ。

## 概要

本プロジェクトは以下の2つのコンポーネントから構成される。

1. **Terminal ライブラリ** — VT100/ANSI エスケープシーケンスのパーサーとターミナル状態マシンの純粋関数的実装
2. **hsterm-gpipe** — GPipe (Haskell 組み込み GPU パイプライン) + GLFW ベースのターミナルエミュレータ GUI

## スクリーンショット

| cmatrix | htop | tig |
|---------|------|-----|
| ![cmatrix](doc/screenshot_cmatrix.png) | ![htop](doc/screenshot_htop.png) | ![tig](doc/screenshot_tig.png) |

## 機能

- VT100/ANSI エスケープシーケンスの包括的サポート
- GPipe による型安全な GPU レンダリングパイプライン
- Haskell で記述可能なカスタムシェーダー（壁紙、背景、テキスト、カーソル）
- Dyre による xmonad スタイルの設定・ホットリロード（`~/.config/haskell-terminal/haskell-terminal.hs`）
- CJK / ワイド文字サポート（fontconfig による自動フォールバック）
- マウスによるテキスト選択とクリップボード操作（Ctrl+Shift+C / V）
- マウストラッキング（X10 / SGR エンコーディング）
- スクロールバック（デフォルト 10,000 行）
- カスタマイズ可能なカラースキーム（GPU アニメーション対応）
- カーソルスタイル選択（ブロック / アンダーライン / バー）
- キーバインディングのカスタマイズ

## ビルド

NixOS / Nix flake を使用（GPipe 関連のローカル依存を含む）：

```bash
# ビルド（Dyre ホットリロード対応版）
nix build

# ビルド（Dyre なし）
nix build .#unwrapped

# 開発シェル
nix develop

# 実行
nix run
# または
./result/bin/hsterm-gpipe
```

Cabal のみで（依存関係が揃っている場合）：

```bash
cabal build
cabal run hsterm-gpipe
```

## テスト

```bash
# Nix 経由
nix build && nix log $(readlink result)

# Cabal 経由
cabal test
```

パーサーテスト（`parser-tests`）とターミナル状態テスト（`terminal-tests`）の2つのテストスイートがある。

## プロジェクト構成

```
Terminal.cabal          -- パッケージ定義
flake.nix               -- Nix flake 設定

src/
├── Terminal/            -- ライブラリ (VT100 エンジン)
│   ├── Types.hs         -- コアデータ型 (Terminal, TerminalChar, TerminalAction, ...)
│   ├── Parser.hs        -- ANSI エスケープシーケンスパーサー (Parsec)
│   ├── ParserUtils.hs   -- Parsec ユーティリティコンビネータ
│   ├── Terminal.hs      -- ターミナル状態マシン (applyAction)
│   ├── Debug.hs         -- ターミナル状態のデバッグ表示
│   └── Posix/           -- POSIX ioctl 関連
│       ├── Winsize.hs   -- struct winsize の FFI バインディング
│       ├── TIOCGWINSZ.hs
│       └── TIOCSWINSZ.hs
├── System/Posix/
│   └── IOCtl.hs         -- ioctl FFI ラッパー (vendored)
├── Hsterm/GPipe/        -- GUI アプリケーション
│   ├── Main.hs          -- メインループ (GPipe ウィンドウ, レンダリング, 入力処理)
│   ├── Config.hs        -- 設定型 (TerminalConfig, CursorStyle, KeyBindings)
│   ├── Shader.hs        -- シェーダー DSL (ShaderConfig, ColorScheme, ColorSlot)
│   ├── Renderer.hs      -- GPipe シェーダーコンパイル, セルデータテクスチャ生成
│   ├── Terminal.hs      -- PTY 管理, シェル起動, UTF-8 デコード
│   ├── Monad.hs         -- HstermM モナド (キーバインド/フック用)
│   ├── Dyre.hs          -- Dyre ホットリロード, 状態シリアライズ
│   └── EntryPoint.hs    -- デフォルトエントリポイント
├── app/
│   └── Main.hs          -- 実行ファイルエントリポイント
└── Hstermplay/
    └── Main.hs          -- script(1) リプレイユーティリティ

tests/
├── parser/Main.hs       -- パーサー単体テスト + QuickCheck
└── terminal/Main.hs     -- ターミナル状態テスト + QuickCheck

data/
├── init.sh              -- ターミナル初期化スクリプト
├── colors.sh            -- ANSI カラーテスト表示
└── fonts/monofur/       -- バンドルフォント (monofur)
```

## アーキテクチャ

### Terminal ライブラリ

ターミナルエミュレーションの核心部分は純粋関数として実装されており、IOに依存しない。

```
入力バイトストリーム
  ↓
Terminal.Parser.parseANSI     -- Parsec で [TerminalAction] にパース
  ↓
Terminal.Terminal.applyAction  -- 純粋な状態遷移: Terminal → TerminalAction → Terminal
  ↓
Terminal.Types.Terminal        -- DiffArray ベースのスクリーンバッファ
```

**主要な型：**

| 型 | 説明 |
|----|------|
| `Terminal` | ターミナル全体の状態（画面、カーソル位置、色、スクロール領域など） |
| `TerminalChar` | 各セルの情報（文字、前景色、背景色、bold/blink/underline/inverse） |
| `TerminalAction` | パース済みの操作（文字入力、カーソル移動、スクロール、属性変更など） |
| `TerminalColor` | 8色（Black〜White）、ANSI SGR コードとの Enum マッピング付き |
| `AttributeMode` | SGR 属性モード（`ResetAllAttributes`, `Foreground c`, `Bright` 等） |

**主要な関数：**

| 関数 | 型 | 説明 |
|------|----|------|
| `parseANSI` | `String → Either ParseError ([TerminalAction], String)` | ANSI ストリームをパースし、アクションと残りの入力を返す |
| `applyAction` | `Terminal → TerminalAction → Terminal` | 純粋な状態遷移関数 |
| `newTerminal` | `(Int, Int) → Maybe TI.Terminal → Terminal` | 指定サイズの空ターミナルを作成 |
| `setSize` | `(Int, Int) → Terminal → Terminal` | リサイズ（アクションバッファのリプレイにより実装） |

### hsterm-gpipe GUI

GPipe + GLFW + gpipe-freetype を使ったターミナルエミュレータの GUI 部分。

```
Shell プロセス (bash 等)
  ↑↓ PTY (疑似端末)
hsterm-gpipe
  ├── TVar Terminal          -- 共有ターミナル状態
  ├── GPipe render loop      -- フレーム描画 (4層シェーダー)
  │   ├── Wallpaper layer    -- オプション壁紙
  │   ├── Background layer   -- セル背景色
  │   ├── Text layer         -- グリフ描画 (gpipe-freetype)
  │   └── Cursor layer       -- カーソル描画
  ├── charCallback           -- Unicode 文字入力 → PTY 送信
  ├── keyCallback            -- 制御キー / キーバインド
  ├── mouseButtonCallback    -- テキスト選択 / マウストラッキング
  └── scrollCallback         -- スクロールバック / マウスホイール
```

**レンダリングアーキテクチャ:**

- 固定グリッド頂点バッファ（リサイズ時のみ再生成）
- 4種のセルデータテクスチャ（bgColor, fgColor, glyphUV, glyphPos）を毎フレーム更新
- シェーダーは全て Haskell で記述し、GPipe が GLSL にコンパイル
- 動的グリフキャッシュ（新出文字のみを差分追加）

## カスタマイズ

### Dyre 設定ファイル

`~/.config/haskell-terminal/haskell-terminal.hs` に Haskell コードで設定を記述する。ファイルを保存して `Ctrl+Shift+R` でホットリロード可能（PTY セッションを維持したまま設定を反映）。

基本的な設定例：

```haskell
import Hsterm.GPipe.Config
import Hsterm.GPipe.Dyre
import Hsterm.GPipe.Main

main :: IO ()
main = hsterm runGPipeTerminal defaultConfig
  { tcFontSize   = 20
  , tcInitialCols = 120
  , tcInitialRows = 36
  , tcCursorStyle = CursorBar
  }
```

### 設定項目

| フィールド | 型 | デフォルト | 説明 |
|------------|-----|-----------|------|
| `tcFontFamily` | `String` | `""` (自動検出) | fontconfig パターン |
| `tcFontSize` | `Int` | `24` | フォントサイズ（ピクセル） |
| `tcColorScheme` | `ColorScheme` | `defaultColorScheme` | 16色カラースキーム |
| `tcShaderConfig` | `ShaderConfig` | `defaultShaderConfig` | シェーダーカスタマイズ |
| `tcDefaultFg` | `TerminalColor` | `White` | デフォルト前景色 |
| `tcDefaultBg` | `TerminalColor` | `Black` | デフォルト背景色 |
| `tcCursorColor` | `V4 Float` | `V4 1.0 0.2 0.9 0.8` | カーソル RGBA |
| `tcCursorStyle` | `CursorStyle` | `CursorBlock` | カーソルスタイル |
| `tcInitialCols` | `Int` | `80` | 初期カラム数 |
| `tcInitialRows` | `Int` | `24` | 初期行数 |
| `tcScrollback` | `Int` | `10000` | スクロールバック行数 |
| `tcShell` | `Maybe FilePath` | `Nothing` | シェルパス（`Nothing` = `$SHELL`） |
| `tcFrameDelay` | `Int` | `16000` | フレーム遅延（μs、60fps ≈ 16000） |
| `tcKeyBindings` | `KeyBindings` | `defaultKeyBindings` | キーバインディング |

### シェーダーカスタマイズ

シェーダーは GPipe の組み込み DSL で記述する。4種類のシェーダーレイヤーがカスタマイズ可能：

| レイヤー | 型 | 説明 |
|----------|-----|------|
| `scWallpaper` | `Maybe WallpaperShaderDef` | フルスクリーン壁紙（`Nothing` = 無効） |
| `scBgShader` | `BgShaderDef` | セル背景の描画 |
| `scTextShader` | `TextShaderDef` | テキストの描画 |
| `scCursorShader` | `CursorShaderDef` | カーソルの描画 |

シェーダー関数には `ShaderGlobals`（時間、解像度、マウス位置、グリッドサイズ、セルサイズ）が渡される。

### カラースキーム

`ColorSlot` でカラーごとに表現を選択できる：

- `SolidColor (V4 Float)` — 静的な色
- `AnimatedColor (Float -> V4 Float)` — CPU 側のフレームごとアニメーション
- `ColorShader (ShaderGlobals -> V2 FFloat -> V2 FFloat -> V4 FFloat)` — GPU フラグメントシェーダー

GPU アニメーション付きカーソルの例：

```haskell
blinkingCursor globals _cellUV baseColor =
  let t     = sgTime globals
      blink = (sin (t * 3.0) + 1.0) * 0.5
      V4 r g b _ = baseColor
  in  V4 r g b (blink * 0.8)
```

### デフォルトキーバインディング

| キー | アクション |
|------|-----------|
| `Ctrl+Q` | 終了 |
| `Ctrl+Shift+C` | 選択テキストをコピー |
| `Ctrl+Shift+V` | クリップボードから貼り付け |
| `Ctrl+Shift+R` | 設定ホットリロード |
| `Shift+PageUp` | 1ページ上にスクロール |
| `Shift+PageDown` | 1ページ下にスクロール |
| `Shift+Home` | スクロールバック先頭へ |
| `Shift+End` | スクロールバック末尾へ |

## ユーティリティ

### hstermplay

`script(1)` で記録したターミナルセッションを再生するCLIツール。

```bash
# 記録
script session.log

# 再生
cabal run hstermplay -- session.log
```

各ANSIアクションとそれを発生させたバイト列を表示し、最後にターミナルの最終画面を出力する。

## ライセンス

GPL-3.0-only。詳しくは [LICENSE](LICENSE) を参照。

## 作者

Hannes Gräuler (hgraeule@uos.de)
