# 開発コマンド

## ビルド

### スタンドアロンバイナリをビルド
```bash
ros build visp.ros
```

### Homebrewでインストール（推奨配布方法）
```bash
brew tap ogrew/visp
brew install visp
```

## テスト

### 完全なテストスイートを実行
```bash
make test
```

### Roswellで直接テスト実行
```bash
ros run --eval "(push #p\"./\" asdf:*central-registry*)" \
        --eval "(ql:quickload :visp/test)" \
        --eval "(rove:run :visp/test :style :spec)"
```

## 開発用REPL

### プロジェクトを開発用REPLに読み込み
```bash
ros run --eval "(push #p\"./\" asdf:*central-registry*)" \
        --eval "(ql:quickload :visp)"
```

## 推奨ワークフロー

### 1. mainから最新を取得
```bash
git checkout main && git pull origin main
```

### 2. 作業用ブランチを作成（命名規則例）
```bash
git checkout -b feature/新機能名
git checkout -b fix/バグ修正名  
git checkout -b docs/ドキュメント更新名
git checkout -b refactor/リファクタリング内容
```

### 3. 作業・コミット・プッシュ
```bash
git add . && git commit -m "適切なコミットメッセージ"
git push -u origin ブランチ名
```

### 4. GitHub でPull Request作成
```bash
gh pr create --title "タイトル" --body "説明"
```

## 段階的開発の原則

- **小さな変更を積み重ねる**: 大きな変更は複数のPRに分割
- **各PR毎にテスト実行**: `make test`で既存機能への影響を確認
- **バイナリビルド確認**: `ros build visp.ros`で実行可能性を検証