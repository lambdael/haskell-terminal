
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
- [ ] 旧 `hsterm`（GLUT版）は互換のため残す → 安定後に削除予定

### TODO
- [ ] ウィンドウリサイズ時のシェーダー再コンパイルを最適化
- [x] バッファの再利用（毎フレーム newBuffer しない）
- [x] 256色 / TrueColor 対応
- [ ] スクロールバック（代替画面バッファ）
- [ ] マウス 対応
- [ ] Xmonad 風ホットリロード対応

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



