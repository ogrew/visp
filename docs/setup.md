# セットアップ手順

## 前提条件

### システム要件
- macOS, Linux, または Windows (WSL推奨)
- ffmpeg がシステムPATHに設定済み
- Git がインストール済み

### ffmpegのインストール

#### macOS (Homebrew)
```bash
brew install ffmpeg
```

#### Ubuntu/Debian
```bash
sudo apt update
sudo apt install ffmpeg
```

#### Windows (Chocolatey)
```bash
choco install ffmpeg
```

## 開発環境セットアップ

### 1. Roswellのインストール

#### macOS (Homebrew)
```bash
brew install roswell
```

#### Linux
```bash
# Ubuntu/Debian
sudo apt install roswell

# または手動インストール
curl -L https://github.com/roswell/roswell/releases/latest/download/roswell_amd64.deb -o roswell.deb
sudo dpkg -i roswell.deb
```

### 2. Common Lisp環境の初期化
```bash
ros setup
ros install sbcl-bin
ros use sbcl-bin
```

### 3. プロジェクトのクローン
```bash
git clone https://github.com/ogrew/visp.git
cd visp
```

### 4. 依存関係の確認
```bash
# ffmpegが利用可能かチェック
ffmpeg -version

# Roswellの動作確認
ros --version
```

## ビルドとテスト

### 開発用ビルド
```bash
# プロジェクトをREPLに読み込み
ros run --eval "(push #p\"./\" asdf:*central-registry*)" \
        --eval "(ql:quickload :visp)"
```

### テスト実行
```bash
# 全テストを実行
make test

# または手動実行
ros run --eval "(push #p\"./\" asdf:*central-registry*)" \
        --eval "(ql:quickload :visp/test)" \
        --eval "(rove:run :visp/test :style :spec)"
```

### プロダクションビルド
```bash
# スタンドアロンバイナリを生成
ros build visp.ros
```

ビルド成功時、実行可能ファイル `visp` が生成されます。

## トラブルシューティング

### ffmpeg not found エラー
```bash
# ffmpegのインストール確認
which ffmpeg

# PATHの設定確認
echo $PATH
```

### Roswell初期化エラー
```bash
# Roswellの再初期化
ros setup
ros install sbcl-bin
```

### テスト失敗
```bash
# 依存関係の再インストール
ros run --eval "(ql:quickload :visp :force t)"
```

### ビルドエラー
```bash
# クリーンビルド
rm -rf ~/.cache/common-lisp/
ros build visp.ros
```

## エディタ設定

### VS Code
推奨拡張機能：
- `commonlisp-vscode` - Common Lisp サポート
- `Parinfer` - 括弧の自動整形

### Emacs
SLIME の設定例：
```elisp
(setq inferior-lisp-program "ros -Q run")
(require 'slime)
(slime-setup '(slime-fancy))
```

### Vim/Neovim
Vlime プラグインの使用を推奨：
```vim
Plug 'vlime/vlime', {'rtp': 'vim/'}
```

## 実行確認

セットアップ完了後、以下のコマンドで動作確認：

```bash
# ヘルプ表示
./visp --help

# ドライラン実行
./visp --input sample.mp4 --res hd --dry-run
```