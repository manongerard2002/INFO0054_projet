sealed trait EtatBinaire
case object Pair extends EtatBinaire
case object Impair extends EtatBinaire
case object SinkBinaire extends EtatBinaire

/**
 * Fonction de transition pour l'AFD des chaines binaires impaires
 *
 * @param etat L'état actuel (Pair ou Impaire)
 * @param symbole Int Le symbole rencontré (0, 1)
 * @return Le nouvel état après la transition
 */
def deltaBinaire(etat: EtatBinaire, symbole: Int): EtatBinaire = (etat, symbole) match
    case (Pair, 0) => Pair
    case (Pair, 1) => Impair
    case (Impair, 0) => Impair
    case (Impair, 1) => Pair
    case _ => SinkBinaire

@main def ChainesBinairesImpaires =
    val ex = AFD(Set(0, 1), deltaBinaire, Pair, Set(Impair))

    println(s"ex.solve() -> ${ex.solve()}")

    val lazySolution = ex.lazysolve()
    println(s"ex.lazysolve().take(4).toList -> ${lazySolution.take(4).toList}")
    
    val mot1: Mot[Int] = List(1, 0, 1, 0, 1, 0, 1, 1)
    println(s"ex.accept($mot1) -> ${ex.accept(mot1)}")
    val mot2: Mot[Int] = List(1, 0, 1, 0, 1, 0, 1, 0)
    println(s"ex.accept($mot2)   -> ${ex.accept(mot2)}")