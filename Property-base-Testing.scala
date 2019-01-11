//Property-base-Testing.scala
trait Prop{
    def check: Either[(FailedCase, SuccessCount), SuccessCount]
    def &&(p: Prop): Prop = new Prop{
        def check = Prop.this.check && p.check()
    }
}

object Prop {
    type FailedCase = String
    type SuccessCount = Int
}

case class Gen[A](sample: State[RNG, A])

//Implement Gen.choose. It should generate integers int eh range start to stopExclusive. Feel
//free ot sue functions you've already written
def choose(start: Int, stopExclusive: Int): Gen[Int] = {
        val (a, rq) = State(gen.sample)
        Gen(State(ints(stopExclusive - start)(rq)))

        Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

}