# Haskellで競技プログラミングをやる

## リンク集

* @hsjoihs, [AtCoder に登録したら解くべき精選過去問 10 問を Haskell で解いてみた – Qiita](https://qiita.com/hsjoihs/items/25a08b426196ab2b9bb0), 2018年3月20日
* @myuon_myon, [Haskellで解くAtCoder – The curse of λ](https://myuon.github.io/posts/haskell-atcoder/), 2019年4月28日
* @hnw, [HaskellでAtCoderの問題を解く（入力の高速化編） – Qiita](https://qiita.com/hnw/items/3f7d27b742c5a1a99a9a), 2019年5月27日
* @mod_poppo, [Haskell で高速なプログラムを書くときに注意すること](https://blog.miz-ar.info/2016/06/writing-efficient-program-with-haskell/), 2016年6月28日
* @mod_poppo, [HaskellでAtCoderに参戦して水色になった](https://blog.miz-ar.info/2019/05/atcoder-with-haskell/), 2019年5月27日

## 入出力

* 数字の列を読み取るのには `map read . words <$> getLine` が使える。
    * ByteStringを使って `map (read . BS.unpack) . BS.words <$> BS.getLine` を使うとより省メモリ。
    * Int または Integer の場合は `unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine` を使うとさらに良い。Vector系にも応用できる。
    * n行読み取る場合は `replicateM n $ do s <- BS.getLine; ...` みたいな感じにする。
* readの際にTypeApplications拡張が欲しい。

コード例は [lib/Input.hs](lib/Input.hs) を参照。

## 正格評価

* 正格評価。サンクを潰そう。
    * タプルの中身に注意。
    * Strict拡張は有用か？

## 配列・ベクター

* ランダムアクセスする用途にリストは使うな。Array / Vector を使え。
    * 1次元ならVector, 2次元以上ならArrayを使う。ただ、2次元以上でもVectorを使う（Vectorを2段重ね）と良いことがある。Vectorには `scan` があったり、タプルのunboxed vectorを作れたりするので。
    * ランダムアクセスせず、fold系操作をするだけならリストでも良い。
    * 固定長整数や浮動小数点数からなる配列にはunboxed array / vector を使う。newtypeと相性が悪いので注意。unboxed vectorの要素型はタプルでも良い。
    * unboxed arrayを使えば通るアルゴリズムが、boxed arrayだと（たとえ正格評価していても）TLEする場合がある。
        * STUArray版 (AC): https://atcoder.jp/contests/dp/submissions/5890874
        * STArray版 (TLE): https://atcoder.jp/contests/dp/submissions/5890929 `writeArray` 時に `$!` で正格評価しているにも関わらずTLEとなった。

`newArray` を使うと型が曖昧になる恐れがある（`IOArray` vs `IOUArray`, `STArray` vs `STUArray`）。
TypeApplications拡張が使えない今は

```haskell
asSTUArray :: ST s (STUArray s i e) -> ST s (STUArray s i e)
asSTUArray = id
```

みたいな補助関数を作って

```haskell
arr <- asSTUArray $ newArray ((0,0),(n,n)) 0
```

という風にするのが精一杯か。

## 全探索

リスト内包表記やリストモナドを使うと、ネストが深くならずに済む。

## モジュラー計算

AtCoderでは「答えを 10^9+7 で割った余りを出力しなさい」という形の問題が頻出である。

モジュラー計算をするのに `addMod` みたいな関数を用意するのもアリなのだろうが、自前のデータ型を用意して `Num` クラスのインスタンスにしておくと、既存の演算子が使いまわせて便利である。また、わざわざ `powMod` を実装しなくてよくなる。

実装は普通にできる（`abs`, `signum` は存在がバグなので右辺は適当に `undefined` にしておく）が、デメリットを挙げるとすれば

* unboxed array / unboxed vector を使うために追加の工夫が必要となる（後述）

であろう。

ABC129-Fのように法が実行時に与えられる場合は、reflectionパッケージ (`Data.Reflection`) と同様の黒魔術 (`unsafeCoerce`) を使うと、実行時に与えられた値を法とするモジュラー計算の型を作ることができる。
実装例は [abc129-f/Main.hs](abc129-f/Main.hs) を参照せよ。

黒魔術に頼りたくないという人は

* @mod_poppo, [型を実行時に作る：怖くないリフレクション - Qiita](https://qiita.com/mod_poppo/items/50ad2c0ee66171cc1ee9), 2017年12月22日

に書いたようなテクニックを使うと良い。ただし、Zero/Succのみで自然数を表現すると値に比例する数のデータ構築子を使うことになってよろしくない。自然数の2進表現を使うと値の桁数に比例する数のデータ構築子で済む（元論文を参照）。

### `rem` vs `mod`

整数を割ったあまりを計算する方法としてHaskell標準には `rem` と `mod` があり、これらはオペランドの符号が異なる場合の挙動が違う。

```haskell
> 7 `mod` 5
2
> (-7) `mod` 5
3
> 7 `mod` (-5)
-3
> (-7) `mod` (-5)
-2
> 7 `rem` 5
2
> (-7) `rem` 5
-2
> 7 `rem` (-5)
2
> (-7) `rem` (-5)
-2
```

実行速度で言うと、 **`rem`の方が速い** ので、どちらでも良い場合（両方のオペランドが非負であるとわかっている場合）は `rem` を使おう。

### `10^9` の定数畳み込み

ソースコードに `10^9` と書いた場合、GHCは `10^9` の定数畳み込みを行わない。
素直にゼロを9個書くという手もあるのだが、ここでは可読性を重視してゼロを9個書かずに済ませる方法を考える。

べき乗関数 `(^)` に関しては指数が小さい時にrewrite ruleによって単純な積へ書き換えられるが、現状は5乗までしか定義されていない。
9乗に関するrewrite ruleを定義してやれば、 `10^9` の定数畳み込みが行われるようになる：

```haskell
{-# RULES
"^9/Int" forall x. x ^ (9 :: Int) = let u = x; v = u * u * u in v * v * v
"^9/Integer" forall x. x ^ (9 :: Integer) = let u = x; v = u * u * u in v * v * v
 #-}
```

最後の行の `#-}` が行頭にあるのとGHC 7.10.3が文句を言うので、空白を開けておくこと。

別の方法としては、NumDecimals拡張を有効にして `1e9` と書くという方法がある。
こちらの方が手軽かもしれない。

NumericUnderscores拡張を使うとゼロが多い整数リテラルを `1_000_000_000` という風に区切り文字を入れて書けるが、GHC 8.6以降というかなり新しいGHCが必要となる。

## 可変な変数

`Int` 1個を保持するのに `IORef` や `STRef` はボックス化のコストがあって効率が悪い。

* 単純な末尾再帰（ループ）の場合は関数の引数か `State` モナド
    * @mod_poppo, [関数内ローカル変数に IORef を使うな](https://qiita.com/mod_poppo/items/03fc14f693b601e0a00f), 2018年2月22日
* 複雑な再帰呼び出しをする場合は1要素のunboxed vector
    * `Data.Vector` の連中はスライスを扱うための追加のフィールドを持っており、その無駄も省きたい人は `Data.Primitive.PrimArray` を使っておくと良さそう。しかし有意な差が出るとは思えない（ので `Data.Vector.Unboxed` で十分か）

を使うと良いだろう。

## unboxed array/vector versus newtype

unboxed array (`UArray`) や unboxed vector (`Data.Vector.Unboxed`) と `newtype` は相性が悪い。
unboxed array/vectorを扱うための型クラス (`IArray` や `Unbox`) に対してはGeneralizedNewtypeDerivingが使えない。

unboxed vectorは

* data family `Data.Vector.Unboxed.Mutable.MVector`
* data family `Data.Vector.Unboxed.Vector`
* type class `Data.Vector.Generic.Mutable.MVector`
* type class `Data.Vector.Generic.Vector`
* type class `Data.Vector.Unboxed.Unbox`

のインスタンスをそれぞれ定義してやれば良い。前者2つは既存のunboxed vectorへのnewtypeで良い。

unboxed arrayはもっと悲惨で、 `UArray` は普通のデータ型として定義されているので自前で型インスタンスを定義するという形にはできない。 `IArray` クラスのインスタンス定義でどうにかする必要がある。
流石にバイト列の読み書きから実装し直すのはだるいので、既存のunboxed arrayの実装を再利用したい。
仮に

```haskell
newtype N = N Int64
```

なるデータ型 `N` に対してunboxed arrayを使えるようにしたいとしよう。
既存のインスタンス定義を再利用するには `UArray ix N` を `UArray ix Int64` にキャストするわけだが、 `UArray` の2番目の型引数のroleはnominalとなっているのでsafe coercionは使えない。そのため、 `unsafeCoerce` を使って

```haskell
unsafeCoerce_UArray_N_Int :: UArray ix N -> UArray ix Int64
unsafeCoerce_UArray_N_Int = Unsafe.Coerce.unsafeCoerce
unsafeCoerce_UArray_Int_N :: UArray ix Int64 -> UArray ix N
unsafeCoerce_UArray_Int_N = Unsafe.Coerce.unsafeCoerce
```

という風なキャスト用のヘルパー関数を用意する。

実際のコード例は

* [lib/UnboxedModularVector.hs](lib/UnboxedModularVector.hs)
* [lib/UnboxedModularArray.hs](lib/UnboxedModularArray.hs)

を参照せよ。

競技プログラミング外で自由にパッケージを使える環境の場合、unboxed vectorに関しては、筆者が作っている [unboxing-vectorパッケージ](https://hackage.haskell.org/package/unboxing-vector) を使うとnewtype時に記述量が少なくて済む。

## IntSet

`IntSet` を舐める際にいちいちリストに変換するのがだるい、という場合は

```haskell
forM_IntSet :: Monad m => IntSet.IntSet -> (Int -> m ()) -> m ()
forM_IntSet set f = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> return ()
               [x] -> forM_ (IntSet.toList x) f
               xs -> forM_ xs go

foldMap_IntSet :: (Monoid n) => (Int -> n) -> IntSet.IntSet -> n
foldMap_IntSet f set = go set
  where
    go set = case IntSet.splitRoot set of
               [] -> mempty
               [x] -> foldMap f (IntSet.toList x)
               xs -> foldMap go xs
```

みたいなやつを用意しておくと `IntSet` の木構造を生かした走査ができる。

[dp-g/Main.hs](dp-g/Main.hs)

## ソート

標準のリストのソート (`Data.List.sort`) は遅い。

標準のリストのソートを使ったせいでTLEとなったケースには筆者は（まだ）遭遇していないが、より高速な代替手段を用意しておくと精神的に楽である。

vector-algorithmsパッケージの各種アルゴリズムが使えると良いのだが、現状使えないようなので自分でソートアルゴリズムを書こう。
実装例は [abc127-d/Main.hs](abc127-d/Main.hs) を参照（この問題は標準のリストのソートでも十分ACできる）。

## その他のデータ構造

* 他の言語のような、破壊的更新のできるハッシュテーブルはなさそう？
    * [hashtables](http://hackage.haskell.org/package/hashtables)
    * Pureなhashmap:
        * hashmap: deprecated
        * [unordered-containers](http://hackage.haskell.org/package/unordered-containers)
* グラフに関するアルゴリズムはどう書くのが良いか（要検討）。

## その他

* 配列の範囲外チェックを省いたりunboxed typeを使うのは（GHCの最適化がすごいので）あまり速度に貢献しないと思う。それよりは、読みやすさ、安全性を優先したい。
    * Vectorのscan系とかfold系とかの関数をうまく使うと自前で添字アクセスすることがなくなる。
* INLINEやSPECIALIZE等のプラグマもあまり意味がなさそう。複数のモジュールからなるプログラムの場合はこれらのプラグマが意味を持つが、AtCoderに投げるHaskellコードは単一のモジュールからなるので。

## AtCoderのGHCが古い問題

GHC 7.10にはSemigroup-Monoid Proposalはおろか、 `Data.Semigroup` が存在しない。
モノイドを使う分には `Data.Monoid` をimportしておけばよいが、自分でモノイドを定義する場合には、最新のGHCではSemigroupがMonoidのスーパークラスとなっているため、GHC 7.10とGHC 8.6の両方で動作するコードを書くにはCPP拡張に頼る必要がある。

例：

```haskell
#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif
#endif
```

```haskell
-- 非負整数に関する Max モノイド
newtype NNInt_Max = NNInt_Max { getMax_NNInt :: Int }
#ifdef MIN_VERSION_base
#if MIN_VERSION_base(4,9,0)
instance Semigroup NNInt_Max where
  NNInt_Max x <> NNInt_Max y = NNInt_Max (max x y)
#endif
#endif
instance Monoid NNInt_Max where
  mempty = NNInt_Max 0
  NNInt_Max x `mappend` NNInt_Max y = NNInt_Max (max x y)
```

完全なソースコードは [dp-g/Main.hs](dp-g/Main.hs) を参照。
