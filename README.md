# Haskell Terminal Emulator

Haskell で書かれた VT100/ANSI ターミナルエミュレータ。ライブラリとGUIアプリケーションの2つの側面を持つ。

## 概要

本プロジェクトは以下の2つのコンポーネントから構成される。

1. **Terminal ライブラリ** — VT100/ANSI エスケープシーケンスのパーサーとターミナル状態マシンの純粋関数的実装
2. **hsterm** — OpenGL + FTGL ベースのターミナルエミュレータ GUI（`xterm` や `gnome-terminal` と同等のもの）

## スクリーンショット

| cmatrix | htop | tig |
|---------|------|-----|
| ![cmatrix](doc/screenshot_cmatrix.png) | ![htop](doc/screenshot_htop.png) | ![tig](doc/screenshot_tig.png) |

## ビルド

NixOS / Nix flake を使用：

```bash
# ビルド
nix build

# 開発シェル
nix develop

# 実行
nix run
# または
./result/bin/hsterm
```

/home/akk/haskell-terminal/haskell-terminal.code-workspace
Cabal のみで（依存関係が揃っている場合）：

```bash
cabal build
cabal run hsterm
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
├── Hsterm/              -- GUI アプリケーション
│   ├── Main.hs          -- エントリポイント
│   ├── Hsterm.hs        -- メインロジック (PTY, OpenGL, GLUT)
│   ├── Config.hs        -- 設定 (色, フォント, レンダラー)
│   ├── State.hs         -- アプリケーション状態
│   ├── Renderer.hs      -- レンダラーインターフェース
│   ├── Theme.hs         -- カラーテーマ
│   ├── LoadShaders.hs   -- GLSL シェーダーローダー
│   ├── Device.hs        -- IODevice インスタンス
│   └── Renderer/        -- レンダラー実装
│       ├── DefaultRenderer.hs
│       ├── Background.hs
│       ├── CursorRenderer.hs
│       ├── FlatColor.hs
│       └── Utils.hs
└── Hstermplay/
    └── Main.hs          -- script(1) リプレイユーティリティ

tests/
├── parser/Main.hs       -- パーサー単体テスト + QuickCheck
└── terminal/Main.hs     -- ターミナル状態テスト + QuickCheck

themes/default/          -- GLSL シェーダー (背景, カーソル, etc.)
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

**設計上の特徴：**

- スクリーンバッファは `DiffArray` で管理。差分のみの更新で効率的
- スクロールは配列のインデックスリマップ（`ixmap`）で実装
- `applyAction` は全アクションを `allBuffer` に蓄積し、リサイズ時にリプレイ可能

### hsterm GUI

OpenGL + GLUT + FTGL を使ったターミナルエミュレータの GUI 部分。

```
Shell プロセス (bash 等)
  ↑↓ PTY (疑似端末)
hsterm
  ├── IORef Terminal      -- 共有ターミナル状態
  ├── GLUT displayCallback  -- フレーム描画
  ├── GLUT keyboardMouseCallback  -- キー入力 → PTY 送信
  └── GLUT reshapeCallback  -- ウィンドウリサイズ → TIOCSWINSZ
```

- 子プロセスは `openPseudoTerminal` で PTY を確保して起動
- PTY からの出力を別スレッドで読み取り、`parseANSI` → `applyAction` で `IORef Terminal` を更新
- 各フレームで `IORef` を読み、FTGL でテキスト描画、シェーダーで背景/カーソルを描画
- カラーテーマは `TerminalConfig` の `colorMap` / `colorMapBright` で関数レベルで差し替え可能
- `Renderer` は `IO (Render, Terminate)` 型で、背景・カーソルの描画をプラグイン可能

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

## カスタマイズ

[src/Hsterm/Main.hs](src/Hsterm/Main.hs) の `myConfig` を編集することで、以下をカスタマイズできる：

- `colorMap` / `colorMapBright` — 8色それぞれの RGB 値
- `fontPath` — TTF フォントのパス
- `fontSize` — フォントサイズ（ピクセル）
- `backgroundRenderer` / `cursorRenderer` — 描画パイプライン

## ライセンス

GPL-3.0-only。詳しくは [LICENSE](LICENSE) を参照。

## 作者

Hannes Gräuler (hgraeule@uos.de)
