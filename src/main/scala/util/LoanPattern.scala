package util

trait LoanPattern {
  def using[A <: {def close()}, B](resource: A)(func: A => B): Option[B] =
    try {
      Some(func(resource)) //成功したら、Someに包んで返す
    } catch {
      case e: Exception => e.printStackTrace()
        None //失敗したら、ログ吐いて、None返す
    } finally {
      if (resource != null) resource.close()
    }


  trait Closer[-A] {
    def close(value: A)
  }

  class Loan[A] private(value: A, closer: Closer[A]) {

    def foreach[B](f: A => B): B = try {
      f(value)
    } finally {
      closer.close(value)
    }

  }

  object Loan {

    def apply[A](value: A)(implicit closer: Closer[A]) = new Loan(value, closer)

    type Closeable = {def close()}
    implicit val closeable = new Closer[Closeable] {
      def close(value: Closeable) = value.close()
    }

    type Releasable = {def release()}
    implicit val destroyable = new Closer[Releasable] {
      def close(value: Releasable) = value.release()
    }

    type Disposable = {def dispose()}
    implicit val disposable = new Closer[Disposable] {
      def close(value: Disposable) = value.dispose()
    }

  }
}
