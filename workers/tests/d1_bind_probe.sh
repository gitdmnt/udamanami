#!/bin/sh
#
# D1 の「1文あたり bind パラメータ 100 個」という上限が、まだ 100 のままかを確かめる。
#
# shared/src/lib.rs のユニットテストが守るのは「コードが定数を超えない」ことだけで、
# 「定数がプラットフォームの実際の上限を超えていない」ことは、実 D1 に投げないと分からない。
# 両方が揃って初めて不変条件が閉じる。
#
# プレースホルダの個数だけで prepare が落ちるので、値をバインドする必要がない。
# 読み取りも書き込みも発生しない(そもそも文が prepare を通らない)。
#
# 実行には wrangler の認証が要るため CI の既定ジョブには入れていない。
# 手で叩くか、認証を渡せる nightly ジョブから呼ぶこと。
#
set -eu

cd "$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)"

DB=udamanami
LIMIT=100

# ?,?,... を n 個作る。
placeholders() {
    awk -v n="$1" 'BEGIN { s = "?"; for (i = 1; i < n; i++) s = s ",?"; print s }'
}

probe() {
    npx wrangler d1 execute "$DB" --remote \
        --command "SELECT 1 WHERE 1 IN ($(placeholders "$1"))" 2>&1 || true
}

fail() {
    echo "FAIL: $1" >&2
    exit 1
}

# 上限ちょうどは通る。値を渡していないので prepare の先で
# "Wrong number of parameter bindings" になるが、これは変数個数検査を
# 通過した証拠なので期待どおり。
at_limit=$(probe "$LIMIT")
case "$at_limit" in
    *"too many SQL variables"*)
        fail "$LIMIT バインドが拒否された。D1 の上限が $LIMIT より小さくなっている。
shared/src/lib.rs の D1_MAX_BOUND_PARAMS を下げること。
$at_limit"
        ;;
esac

# 上限+1 は必ず落ちる。この逆向きの検査が無いと、認証失敗や
# エラー文字列の変更で空振りしたまま緑になる。
over=$(probe "$((LIMIT + 1))")
case "$over" in
    *"too many SQL variables"*) ;;
    *)
        fail "$((LIMIT + 1)) バインドが拒否されなかった。上限が緩んだか、
プローブ自体が壊れている(認証切れなど)。出力を確認すること。
$over"
        ;;
esac

echo "OK: D1 の bind パラメータ上限は $LIMIT のまま"
