# リファクタリングタスク

## 高優先度（品質・安定性の改善）

### validate-merge-files関数の分割
**対象**: `validate.lisp:41`の`validate-merge-files`関数（114行）

**問題**: 単一の関数が複数の責任を持ち、可読性・保守性が低下

**改善策**:
- ファイル存在・形式チェック関数の分離 (`validate-merge-files-basic`)
- 音声・解像度・fps互換性チェック関数の分離 (`validate-merge-compatibility`)
- メタデータ取得・検証の分離 (`extract-and-validate-metadata`)

### 命名の一貫性改善
**対象**: `options.lisp:11`と`options.lisp:51`

**問題**: `repeat`フィールドと`--loop`オプション名の不整合

**改善策**:
- フィールド名を`loop`に統一し、関連する関数名も変更
- `validate-repeat` → `validate-loop`への変更も必要

### エラーハンドリングの詳細化
**対象**: `ffmpeg.lisp:33`の`run-cmd`関数

**現状**: 基本的なエラーハンドリングは実装済み（`validate-output-path`、`cleanup-partial-output`）

**追加改善:**
- ffmpeg実行失敗時のstderr詳細取得と表示
- エラーコード別の対処方法ガイダンス表示
- 権限エラーの具体的な解決策提示

### テスト改善

#### エラーケーステストの改善
**現在の問題:**
- validate系関数のエラーケースがテストされていない
- `(uiop:quit 1)`によるプロセス終了がテストフレームワークと非互換
- テストカバレッジが不十分で潜在的バグの発見が困難

**改善アプローチ（段階的実装推奨）:**
1. 1つの関数から例外ベースに段階的変更
2. 各段階でのバイナリビルド動作確認
3. エラーケーステストの段階的追加
4. リグレッション防止の強化

**注意**: 大規模な一括変更は避け、小さな改善を積み重ねる方式を採用する。

#### 統合テストの充実
**追加すべきテスト:**
- 実際のffmpeg実行を含むエンドツーエンドテスト
- エラーケースの異常系テスト
- バッチ処理でのファイル競合テスト

## 中優先度（パフォーマンス・保守性の向上）

### パフォーマンス最適化

#### ffprobeキャッシュ機能
**対象**: `video.lisp`の各メタデータ取得関数

**問題**: 同じファイルの重複解析でパフォーマンス低下

**改善策**:
- ファイルパス＋更新日時をキーとしたキャッシュ実装
- メモリ使用量制限付きのLRUキャッシュ

#### バッチ処理の並列化
**対象**: `main.lisp:35`のバッチモード処理

**改善策**:
- 複数ファイルの並列処理対応
- CPU数に基づく適切な並列度制御

### コード品質改善

#### 関数レベルdocstring追加
**対象**: 特に複雑なロジック関数

**優先対象**:
- `build-concat-filter` (ffmpeg.lisp:71) - 複雑なフィルタ文字列生成
- `parse-args-to-options` (options.lisp:25) - 長いオプション解析ループ
- `generate-output-filename` (util.lisp:76) - 複雑なファイル名生成ロジック

#### マジックナンバーの定数化
**対象**: `ffmpeg.lisp:59`のGIF fps計算

**現状**: `(/ fps 2.0)` でハードコード

**改善策**:
- `+gif-fps-divider+` 定数を `const.lisp` に追加
- GIFサイズ調整の設定可能化

## 低優先度（新機能追加）

### 日常利便性機能

#### プリセット機能
```bash
visp --save-preset mobile-hd --res hd --fps 30 --codec h264
visp --preset mobile-hd --input video.mp4
```
- `~/.visp/presets.json`での設定管理
- プリセット一覧表示 (`--list-presets`)

#### 品質設定オプション
```bash
visp --input video.mp4 --quality high    # CRF 18相当
visp --input video.mp4 --quality medium  # CRF 23相当
visp --input video.mp4 --quality low     # CRF 28相当
```
- 内部的にはCRF値やビットレート調整
- コーデック別の最適化

#### プログレス表示機能
```bash
visp --input large-video.mp4 --res 4k --progress
# [████████████████████████████████] 85% (02:45 remaining)
```
- ffmpegの進捗パース
- 残り時間推定アルゴリズム

### 実用性向上機能

#### バッチ処理改善
```bash
visp --input /videos --recursive --include "*.mp4,*.mov"
```
- サブディレクトリ再帰処理
- 拡張子フィルタリング
- 処理結果サマリー表示

#### メタデータ保持オプション
```bash
visp --input video.mp4 --res fhd --keep-metadata
```
- 元ファイルのタイトル、作成日時等の保持
- メタデータの選択的保持・削除

#### 出力ファイル名テンプレート
```bash
visp --output-template "%{name}_%{resolution}_%{date}.%{ext}"
# video_1920x1080_20241225.mp4
```
- カスタマイズ可能な出力ファイル名
- 変数展開（解像度、日付、元ファイル名等）

### 高度機能

#### カスタムフィルタ機能
```bash
visp --input video.mp4 --filters "blur=3,sharpen=1,fade=in:0:30"
```
- フィルタチェーンのカスタマイズ
- よく使うフィルタのエイリアス定義

#### プレビュー機能
```bash
visp --input video.mp4 --res fhd --preview
```
- 最初の5秒間の変換結果をプレビュー生成
- サムネイル生成機能

## その他（開発環境・ドキュメント）

### ドキュメント改善
- 内部関数のdocstring充実
- アーキテクチャ図の追加
- エラー解決ガイドの作成
- パフォーマンス指標の文書化

### 開発環境整備
- CI/CDパイプラインでの自動テスト
- コードカバレッジ測定
- リンター設定の強化
- 依存関係の自動更新

### 国際化対応
- エラーメッセージの統一（日英混在の解決）
- ヘルプメッセージの改善
- 多言語対応の基盤整備