import scala.io.StdIn
sealed trait EtatBinaire
case object Pair extends EtatBinaire
case object Impair extends EtatBinaire

/**
 * Fonction de transition pour l'AFD des chaines binaires impaires
 *
 * @param etat L'état actuel (Pair ou Impaire)
 * @param symbole Int Le symbole rencontré (0, 1)
 * @return Un Option du nouvel état après la transition
 */
def deltaBinaire(etat: EtatBinaire, symbole: Int): Option[EtatBinaire] = (etat, symbole) match
    case (Pair, 0) => Some(Pair)
    case (Pair, 1) => Some(Impair)
    case (Impair, 0) => Some(Impair)
    case (Impair, 1) => Some(Pair)
    case _ => None

@main def ChainesBinairesImpaires =
    val ex = new AFD(Set(0, 1), deltaBinaire, Pair, Set(Impair))

    println(s"ex.solve() -> ${ex.solve()}")

    val lazySolution = ex.lazysolve()
    println(s"ex.lazysolve().take(4).toList -> ${lazySolution.take(4).toList}")
    
    val mot1: Mot[Int] = List(1, 0, 1, 0, 1, 0, 1, 1)
    println(s"ex.accept($mot1) -> ${ex.accept(mot1)}")
    val mot2: Mot[Int] = List(1, 0, 1, 0, 1, 0, 1, 0)
    println(s"ex.accept($mot2) -> ${ex.accept(mot2)}")

    println("Veuillez entrer une chaîne binaire (tous les caractères accolés) pour déterminer si elle est impaire:")
    println(s"L'entrée est acceptée -> ${ex.parseAccept(StdIn.readLine(), (x: String) => x.split("").toList.map(_.toInt))}")
