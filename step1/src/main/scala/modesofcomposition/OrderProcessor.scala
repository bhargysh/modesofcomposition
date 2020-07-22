package modesofcomposition

object OrderProcessor {

  def decodeMsg[F[_]: ApplicativeError[*[_], Throwable]](msg: Array[Byte]): F[OrderMsg] =
    errorValueFromEither[F](parser.decode[OrderMsg](new String(msg)))
//  errorValueFromEither contructs a new ErrorValueFromEitherPartiallyApplied with a effectful type F[_]
//  the apply method of ErrorValueFromEitherPartiallyApplied takes an e and implicit ApplicativeError
//  e: => Either[E,A] which is the return type of decode: Either[Error, A]
//  and we have an implicit ApplicativeError on our method

}

