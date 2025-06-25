# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 言語設定

Claude Codeとのやり取りは**日本語**で行ってください。コードコメントやドキュメント作成時も日本語を使用してください。

## ドキュメント構成

このプロジェクトのドキュメントは以下のように構成されています：

### 基本ドキュメント
- **[プロジェクト概要](docs/project-overview.md)** - Vispの目的、機能、設計思想
- **[セットアップ手順](docs/setup.md)** - 開発環境の構築とインストール方法
- **[開発コマンド](docs/development-commands.md)** - ビルド、テスト、ワークフローコマンド

### 技術ドキュメント
- **[アーキテクチャ](docs/architecture.md)** - システム設計、コンポーネント構成、データ構造
- **[開発ガイドライン](docs/development-guidelines.md)** - コーディング規約、ブランチ運用、PR作成手順
- **[リファクタリングタスク](docs/refactoring-tasks.md)** - 改善予定項目と優先度

## 重要な指針

### 開発原則
- **mainブランチは保護されており、直接コミットできません**
- すべての作業は専用のfeatureブランチで行う必要があります
- 変更はPull Request経由でのみmainにマージ可能です
- 小さな変更を積み重ねる段階的開発を推奨

### テストとビルド
- 新機能追加時は必ず `make test` でテスト実行
- ビルド確認は `ros build visp.ros` で実施
- エラーケーステストの改善が重要な課題

### 関数名の重複チェック
新しい関数定義時は以下を確認：
1. `grep -r "defun 関数名" src/` で既存関数チェック
2. `src/package.lisp` でエクスポート重複チェック
3. テスト実行で既存機能への影響確認

**注意**: Common Lispでは同名関数の再定義が警告なしに行われるため特に注意が必要です。
