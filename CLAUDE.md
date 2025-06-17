# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 言語設定

Claude Codeとのやり取りは**日本語**で行ってください。コードコメントやドキュメント作成時も日本語を使用してください。

## プロジェクト概要

VispはffmpegのミニマルなCommon Lispラッパーです。動画の解像度変換、コーデック変換、音声除去などの一般的な動画処理を簡素化し、出力ファイル名を自動生成します。

## 開発コマンド

### ビルド
```bash
# スタンドアロンバイナリをビルド
ros build visp.ros

# Homebrewでインストール（推奨配布方法）
brew tap ogrew/visp
brew install visp
```

### テスト
```bash
# 完全なテストスイートを実行
make test

# Roswellで直接テスト実行
ros run --eval "(push #p\"./\" asdf:*central-registry*)" \
        --eval "(ql:quickload :visp/test)" \
        --eval "(rove:run :visp/test :style :spec)"
```

### 開発用REPL
```bash
# プロジェクトを開発用REPLに読み込み
ros run --eval "(push #p\"./\" asdf:*central-registry*)" \
        --eval "(ql:quickload :visp)"
```

## アーキテクチャ

### 核となるコンポーネント
- **main.lisp**: エントリーポイントと実行フローの制御
- **options.lisp**: CLI引数解析とvisp-options構造体
- **validate.lisp**: 入力検証とオプション互換性チェック
- **ffmpeg.lisp**: FFmpegコマンド構築と実行
- **video.lisp**: ffprobeを使用した動画メタデータ抽出
- **util.lisp**: ファイル名生成とパースユーティリティ
- **const.lisp**: 解像度マップ、コーデック設定、ファイル拡張子

### 実行フロー
1. **初期化**: ffmpeg可用性チェック、CLI引数をvisp-options構造体に解析
2. **検証**: モード（通常/マージ/GIF/バッチ）に基づく検証ディスパッチ
3. **モード選択**: 適切な処理モードへのルーティング
4. **コマンド構築**: 検証済みオプションに基づくffmpegコマンド生成
5. **実行**: ffmpeg実行またはドライラン出力表示

### 処理モード
- **通常モード**: 単一ファイル処理（各種変換オプション適用）
- **バッチモード**: ディレクトリ処理（入力がディレクトリの場合自動）
- **マージモード**: `--merge`で複数MP4ファイル結合
- **GIFモード**: `--gif`でアニメーションGIF変換

### 主要データ構造
- **visp-options構造体**: 全解析済みオプションを含む中央設定オブジェクト
- **解像度定数**: 事前定義マッピング（hd=1280x720、fhd=1920x1080など）
- **コーデックマップ**: 拡張子とピクセルフォーマット付きエンコーダー設定

## 依存関係

### ランタイム
- **UIOP**: 唯一のCommon Lisp依存関係（SBCLに含まれる）
- **ffmpeg/ffprobe**: システムPATHで利用可能である必要がある

### 開発環境
- **SBCL + Roswell**: ビルドとRELP開発用
- **Rove**: テストフレームワーク（Quicklisp経由で読み込み）

## テストフレームワーク

`t/`ディレクトリのテストファイルでRoveテストライブラリを使用。ユーティリティ関数、ffmpegコマンド構築、出力処理をカバー。