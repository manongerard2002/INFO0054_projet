import scala.io.StdIn
/**
 * Enumération représentant une pièce du taquin
 * - Trou représente la case vide/le trou
 * - Nombre(valeur) représente une case avec une valeur qui est un Int
 */
enum Piece:
    case Trou
    case Nombre(valeur: Int)

/**
 * Fonction qui convertit une chaîne de caractères en une grille de Taquin
 *
 * @param input Une chaîne de caractères représentant une grille de Taquin sous forme de tableau
 *              La case vide est représentée par un "_"
 * @return Une liste de listes de pièces, représentant la grille du Taquin
 *         Si le format d'entrée est invalide, renvoie une liste vide
 * @throws IllegalArgumentException si la chaîne d'entrée ne respecte pas le format attendu 
 *                                  ou s'il n'y a pas exactement une case vide
 */
def parseTaquin(input: String): List[List[Piece]] =
    val cleanedInput = input.replaceAll("\\s+", " ").replaceAll("\\[ ", "[").replaceAll(" \\]", "]").trim

    /**
     * Fonction interne qui analyse une ligne de la grille de Taquin et la convertit en une liste de pièces (Piece)
     *
     * @param row Une chaîne de caractères représentant une ligne de la grille, avec des nombres séparés par des espaces 
     *            et "_" représentant la case vide
     * @return Une liste de pièces (Piece) correspondant à la ligne de la grille
     * @throws IllegalArgumentException Si une des cases ne peut pas être convertie en un nombre valide
     */
    def parseRow(row: String): List[Piece] =
        row.split(" ").toList.map {
            case "_" => Piece.Trou
            case nombre => 
                try Piece.Nombre(nombre.toInt)
                catch case _: NumberFormatException => throw new IllegalArgumentException(s"Format de Nombre invalide pour la valeur : ${nombre}")
        }
    try
        if (!(cleanedInput.startsWith("[[") && cleanedInput.endsWith("]]")))
            throw new IllegalArgumentException("Format invalide : l'entrée doit commencer par '[[' et finir par ']]'")

        val underscoreCount = input.count(_ == '_')
        if (underscoreCount != 1)
            throw new IllegalArgumentException("Format invalide : l'entrée doit contenir exactement un '_' représentant la case vide (Trou)")

        val gridRows = cleanedInput.stripPrefix("[[").stripSuffix("]]").split("\\]\\[").toList.map(parseRow)
        val gridSize = gridRows.size
        if (!gridRows.forall(_.size == gridSize))
            throw new IllegalArgumentException("Format invalide : l'entrée doit représenter une grille carrée (NxN)")

        val Nombres = gridRows.flatten.collect { case Piece.Nombre(valeur) => valeur }
        val expectedNombres = (1 to (gridSize * gridSize - 1)).toSet
        val actualNombres = Nombres.toSet
        if (actualNombres != expectedNombres)
            throw new IllegalArgumentException(s"Format invalide : l'entrée doit contenir des Nombres uniques de 1 à ${gridSize * gridSize - 1}")

        gridRows
    catch
        case e: IllegalArgumentException =>
            println(e.getMessage)
            Nil
        case _: Exception =>
            println("An unexpected error occurred while parsing.")
            Nil

/**
 * Classe représentant l'état du Taquin sous forme d'une grille
 *
 * @param grille La grille actuelle du Taquin sous forme de liste de listes de Piece
 */
case class Taquin(grille: List[List[Piece]]):
    val sigma = Set("u", "d", "l", "r")
    val N = grille.size

    lazy val etatFinal = Taquin((1 until N * N).foldRight(List(Piece.Trou: Piece))((n, acc) => Piece.Nombre(n) :: acc).grouped(N).toList)

    /**
     * Affiche la grille du Taquin sous une forme lisible
     *
     * @return Une représentation en chaîne de caractères de la grille où Trou est représenté par "_"
     */
    override def toString: String = grille.map { row =>
        "[" + row.map {
            case Piece.Trou => "_"
            case Piece.Nombre(n) => n.toString
        }.mkString(" ") + "]"
    }.mkString("[", "", "]")

    /**
     * Trouve la position de la case vide dans la grille
     *
     * @return Un Option du tuple (row, col) représentant les coordonnées de la 1e case vide dans la grille
     */
    def findTrou: Option[(Int, Int)] =
        grille.zipWithIndex.flatMap { case (row, rowIndex) =>
            row.zipWithIndex.collect {
                case (Piece.Trou, colIndex) => (rowIndex, colIndex)
            }
        }.headOption

    /**
     * Retourne un nouveau taquin où l'on a appliqué un mouvement sur la grille du Taquin en fonction de l'action spécifiée
     *
     * @param action Le mouvement du trou: - "u" pour vers le haut
     *                                     - "d" pour vers le bas
     *                                     - "l" pour vers la gauche
     *                                     - "r" pour vers la droite
     * @return Un Option[Taquin] contenant le nouveau taquin obtenu après mouvement
     */
    def move(action: String): Option[Taquin] =
        val trou = findTrou

        if (trou.isDefined)
            val (trouRow, trouCol) = trou.get

            def swapTrou(row: Int, col: Int): List[List[Piece]] =
                val grille2 = grille.updated(trouRow, grille(trouRow).updated(trouCol, grille(row)(col)))
                grille2.updated(row, grille2(row).updated(col, Piece.Trou))

            action match
                case "u" if trouRow > 0 => Some(Taquin(swapTrou(trouRow - 1, trouCol)))
                case "d" if trouRow < N - 1 => Some(Taquin(swapTrou(trouRow + 1, trouCol)))
                case "l" if trouCol > 0 => Some(Taquin(swapTrou(trouRow, trouCol - 1)))
                case "r" if trouCol < grille(0).length - 1 => Some(Taquin(swapTrou(trouRow, trouCol + 1)))
                case _ => None
        else
            None

    /**
     * Heuristique qui retourne le nombre de pièces mal placées par rapport à l'état final.
     *
     * @return Un entier représentant le nombre de pièces mal placées.
     */
    def H1: Int =
        grille.flatten.zipWithIndex.count {
            case (Piece.Nombre(valeur), idx) => valeur != idx + 1
            case (Piece.Trou, _) => false
        }

    /**
     * Heuristique qui calcule la somme des distances de Manhattan de chaque pièce à sa position finale.
     *
     * @return Un entier représentant la somme des distances de Manhattan.
     */
    def H2: Int =
        grille.flatten.zipWithIndex.map {
            case (Piece.Nombre(valeur), idx) =>
                val correctRow = (valeur - 1) / N
                val correctCol = (valeur - 1) % N
                val currentRow = idx / N
                val currentCol = idx % N
                Math.abs(correctRow - currentRow) + Math.abs(correctCol - currentCol)
            case (Piece.Trou, _) => 0
        }.sum

@main def main =
    /**
     * Fonction de transition pour la grille du Taquin en fonction de l'action spécifiée par le symbole
     *
     * @param etat Le taquin actuel
     * @param action Le mouvement du trou: - "u" pour vers le haut
     *                                     - "d" pour vers le bas
     *                                     - "l" pour vers la gauche
     *                                     - "r" pour vers la droite
     * @return Un Option[Taquin] contenant la nouvelle grille après mouvement
     */
    def delta(etat: Taquin, symbole: String): Option[Taquin] = etat.move(symbole)

    /**
     * Heuristique qui retourne le nombre de pièces mal placées par rapport à l'état final pour le taquin donné.
     *
     * @param taquin Le taquin
     * @return Un entier représentant le nombre de pièces mal placées.
     */
    def H1(taquin: Taquin): Int = taquin.H1

    /**
     * Heuristique qui calcule la somme des distances de Manhattan de chaque pièce à sa position finale pour le taquin donné.
     *
     * @param taquin Le taquin
     * @return Un entier représentant la somme des distances de Manhattan.
     */
    def H2(taquin: Taquin): Int = taquin.H2
    
    val taquin1 = Taquin(parseTaquin("[[ _ 2][1 3]]"))
    val ex1 = new AFD(taquin1.sigma, delta, taquin1, Set(taquin1.etatFinal))
    println(s"Tests pour le taquin : ${taquin1}")
    println(s"ex1.solve() -> ${ex1.solve()}")
    /*println(s"ex1.solveHeuristique(H1) -> ${ex1.solveHeuristique(H1)}")
    println(s"ex1.solveHeuristique2(H1) -> ${ex1.solveHeuristique2(H1)}")
    println(s"ex1.solveHeuristique(H2) -> ${ex1.solveHeuristique(H2)}")
    val lazySolution1 = ex1.lazysolve()
    println(s"ex.lazysolve().take(1).toList -> ${lazySolution1.take(1).toList}")
    println(s"ex.lazysolve().take(2).toList -> ${lazySolution1.take(2).toList}")
    val lazySolution1H1 = ex1.lazysolveHeuristique(H1)
    println(s"ex1.lazysolveHeuristique().take(1).toList -> ${lazySolution1H1.take(1).toList}")
    println(s"ex1.lazysolveHeuristique().take(2).toList -> ${lazySolution1H1.take(2).toList}")*/

    val mot1: Mot[String] = List("d", "u", "d", "r")
    println(s"ex1.accept($mot1) -> ${ex1.accept(mot1)}")

    val taquin2 = Taquin(parseTaquin("[[2 1][3 _]]"))
    val ex2 = new AFD(taquin2.sigma, delta, taquin2, Set(taquin2.etatFinal))
    println(s"Tests pour le taquin : ${taquin2}")
    println(s"ex2.solve() -> ${ex2.solve()}")
    /*println(s"ex2.solveHeuristique(H1) -> ${ex2.solveHeuristique(H1)}")
    println(s"ex2.solveHeuristique(H2) -> ${ex2.solveHeuristique(H2)}")*/

    val taquin3 = Taquin(parseTaquin("[[2 3 6][1 _ 5][7 8 4]]"))
    val ex3 = new AFD(taquin3.sigma, delta, taquin3, Set(taquin3.etatFinal))
    println(s"Tests pour le taquin : ${taquin3}")
    /*println(s"ex3.solve()                 -> ${ex3.solve()}")
    println(s"ex3.solveHeuristique(H1) -> ${ex3.solveHeuristique(H1)}")
    println(s"ex3.solveHeuristique(H2) -> ${ex3.solveHeuristique(H2)}")*/
    val lazySolution3H1 = ex3.lazysolveHeuristique(H1)
    val mot3 = lazySolution3H1.take(1).toList.head
    println(s"ex3.lazysolveHeuristique().take(1).toList -> ${lazySolution3H1.take(1).toList}")
    println(s"ex3.lazysolveHeuristique().take(2).toList -> ${lazySolution3H1.take(2).toList}")
    // Qu'est ce que le prof veut quand il parle de montrer le résultat de solve, car il a l'air tres long

    val mot2: Mot[String] = List(
            "r", "u", "l", "d", "l", "u", "r", "d", "l", "u", "r", "r", "d", "l", "u", "r", "d", "l", "u", "r", "d", "d", "l", "u", "r", "d", "l", "u", "r", "d", 
            "l", "l", "u", "r", "d", "l", "u", "r", "d", "r", "u", "l", "l", "d", "r", "r", "u", "l", "l", "d", "r", "r", "u", "l", "d", "r", "u", "l", "d", "l", "u",
            "r", "r", "d", "l", "u", "r", "d", "l", "u", "l", "d", "r", "u", "r", "d", "l", "u", "r", "d", "l", "l", "u", "r", "r", "d", "l", "l", "u", "r", "d", "l", "u", "r", "d", "r"
        )
    println(s"ex3.accept($mot2) -> ${ex3.accept(mot2)}")

    val startTimeH1_1 = System.nanoTime
    val lazySolutionH1_1 = ex3.lazysolveHeuristique(H1)
    val solutionH1_1 = lazySolutionH1_1.take(1).toList
    val endTimeH1_1 = System.nanoTime
    println(s"Temps de résolution avec H1 pour ${taquin3} afin d'avoir 1 solutions : ${(endTimeH1_1 - startTimeH1_1) / 1000000} ms")

    val startTimeH2_1 = System.nanoTime
    val lazySolutionH2_1 = ex3.lazysolveHeuristique(H2)
    val solutionH2_1 = lazySolutionH2_1.take(1).toList
    val endTimeH2_1 = System.nanoTime
    println(s"Temps de résolution avec H2 pour ${taquin3} afin d'avoir 1 solutions : ${(endTimeH2_1 - startTimeH2_1) / 1000000} ms")

    val startTimeH1_2 = System.nanoTime
    val lazySolutionH1_2 = ex3.lazysolveHeuristique(H1)
    val solutionH1_2 = lazySolutionH1_2.take(5).toList
    val endTimeH1_2 = System.nanoTime
    println(s"Temps de résolution avec H1 pour ${taquin3} afin d'avoir 5 solutions : ${(endTimeH1_2 - startTimeH1_2) / 1000000} ms")

    val startTimeH2_2 = System.nanoTime
    val lazySolutionH2_2 = ex3.lazysolveHeuristique(H2)
    val solutionH2_2 = lazySolutionH2_2.take(5).toList
    val endTimeH2_2 = System.nanoTime
    println(s"Temps de résolution avec H2 pour ${taquin3} afin d'avoir 5 solutions : ${(endTimeH2_2 - startTimeH2_2) / 1000000} ms")

    val startTimeH1_3 = System.nanoTime
    val lazySolutionH1_3 = ex3.lazysolveHeuristique(H1)
    val solutionH1_3 = lazySolutionH1_3.take(10).toList
    val endTimeH1_3 = System.nanoTime
    println(s"Temps de résolution avec H1 pour ${taquin3} afin d'avoir 10 solutions : ${(endTimeH1_3 - startTimeH1_3) / 1000000} ms")

    val startTimeH2_3 = System.nanoTime
    val lazySolutionH2_3 = ex3.lazysolveHeuristique(H2)
    val solutionH2_3 = lazySolutionH2_3.take(10).toList
    val endTimeH2_3 = System.nanoTime
    println(s"Temps de résolution avec H2 pour ${taquin3} afin d'avoir 10 solutions : ${(endTimeH2_3 - startTimeH2_3) / 1000000} ms")

    println(s"Veuillez entrer une suite de u, d, l et r (tous les caractères accolés) pour déterminer si c'est une solution du taquin ${taquin3}:")
    println(s"L'entrée est acceptée -> ${ex3.parseAccept(StdIn.readLine(), (x: String) => x.split("").toList)}")
