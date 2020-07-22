package modesofcomposition

object OrderProcessor {

  //resolveOrderMsg has been broken down into named parts to help understand the pieces of the computation
  def resolveOrderMsg[F[_]: Sync: Parallel: SkuLookup: CustomerLookup](msg: OrderMsg): F[CustomerOrder] =
    msg match { case OrderMsg(custIdStr, items) =>

      val resolveCustomer: F[Customer] = {
        val fofEitherStrOrCust: F[Either[String, Customer]] = CustomerLookup[F].resolveCustomerId(custIdStr)
        val fOfCustomer: F[Customer] = fofEitherStrOrCust.flatMap(eitherStringOrCust => errorValueFromEither[F](eitherStringOrCust))
        fOfCustomer
      }

      val resolveSkuQuantity: ((String, Int)) => F[SkuQuantity] = {
        case (skuCode, quantity) => {
          val fOfSku: F[Sku] = SkuLookup[F].resolveSku(skuCode).flatMap(eitherStrOrSku => errorValueFromEither[F](eitherStrOrSku))
          val fOfPosInt: F[PosInt] = PosInt.fromF[F](quantity) // same as F[Refined[PosInt, Int]]
//          (fOfSku, fOfPosInt).mapN(SkuQuantity) this works but is not resolved in parallel, use parMapN
          (fOfSku, fOfPosInt).parMapN(SkuQuantity)
        }
      }

      val resolveSkus: F[NonEmptyChain[SkuQuantity]] = {
//        items.map(tuple => resolveSkuQuantity(tuple)).sequence same as doing traverse, works but also not in parallel
        items.parTraverse(resolveSkuQuantity)
      }

      //applicative composition
      (
        resolveCustomer,
        resolveSkus,
        ).parMapN(CustomerOrder(_, _))
    }
}

