/**
 * Enumération représentant une pièce du taquin
 * - Empty représente la case vide
 * - Number(value) représente une case avec un numéro
 */
enum Piece:
    case Empty
    case Number(value: Int)

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
            case "_" => Piece.Empty
            case numberStr => 
                try Piece.Number(numberStr.toInt)
                catch case _: NumberFormatException => throw new IllegalArgumentException(s"Invalid number format for value: '$numberStr'")
        }
    try
        if (!(cleanedInput.startsWith("[[") && cleanedInput.endsWith("]]")))
            throw new IllegalArgumentException("Invalid format: input must start with '[[' and end with ']]'")

        val underscoreCount = input.count(_ == '_')
        if (underscoreCount != 1)
            throw new IllegalArgumentException("Invalid format: input must contain exactly one '_' representing Empty")

        val gridRows = cleanedInput.stripPrefix("[[").stripSuffix("]]").split("\\]\\[").toList.map(parseRow)
        val gridSize = gridRows.size
        if (!gridRows.forall(_.size == gridSize))
            throw new IllegalArgumentException("Invalid format: input must represent a square grid (NxN)")

        val numbers = gridRows.flatten.collect { case Piece.Number(value) => value }
        val expectedNumbers = (1 to (gridSize * gridSize - 1)).toSet
        val actualNumbers = numbers.toSet
        if (actualNumbers != expectedNumbers)
            throw new IllegalArgumentException(s"Invalid format: input must contain unique numbers from 1 to ${gridSize * gridSize - 1}")

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

    lazy val solutionGrille: List[List[Piece]] =
        val grilleFinalle = (1 until N * N).foldRight(List(Piece.Empty: Piece))((num, acc) => Piece.Number(num) :: acc)
        grilleFinalle.grouped(N).toList

    lazy val etatFinal = Taquin(solutionGrille)

    /**
     * Affiche la grille du Taquin sous une forme lisible
     *
     * @return Une représentation en chaîne de caractères de la grille où Empty est représenté par "_"
     */
    override def toString: String = grille.map { row =>
        "[" + row.map {
            case Piece.Empty => "_"
            case Piece.Number(n) => n.toString
        }.mkString(" ") + "]"
    }.mkString("[", "", "]")

    /**
     * Trouve la position de la case vide dans la grille
     *
     * @return Un Option du tuple (row, col) représentant les coordonnées de la 1e case vide dans la grille
     */
    def findEmpty: Option[(Int, Int)] =
        grille.zipWithIndex.flatMap { case (row, rowIndex) =>
            row.zipWithIndex.collect {
                case (Piece.Empty, colIndex) => (rowIndex, colIndex)
            }
        }.headOption

    /**
     * Applique un mouvement sur la grille du Taquin en fonction de l'action spécifiée
     *
     * @param action Le mouvement du trou: - "u" pour vers le haut
     *                                     - "d" pour vers le bas
     *                                     - "l" pour vers la gauche
     *                                     - "r" pour vers la droite
     * @return Un Option[Taquin] contenant la nouvelle grille après mouvement
     */
    def move(action: String): Option[Taquin] =
        val empty = findEmpty

        if (empty.isDefined)
            val (emptyRow, emptyCol) = empty.get

            def swapEmpty(row: Int, col: Int): List[List[Piece]] =
                val grille2 = grille.updated(emptyRow, grille(emptyRow).updated(emptyCol, grille(row)(col)))
                grille2.updated(row, grille2(row).updated(col, Piece.Empty))

            action match
                case "u" if emptyRow > 0 => Some(Taquin(swapEmpty(emptyRow - 1, emptyCol)))
                case "d" if emptyRow < N - 1 => Some(Taquin(swapEmpty(emptyRow + 1, emptyCol)))
                case "l" if emptyCol > 0 => Some(Taquin(swapEmpty(emptyRow, emptyCol - 1)))
                case "r" if emptyCol < grille(0).length - 1 => Some(Taquin(swapEmpty(emptyRow, emptyCol + 1)))
                case _ => None
        else
            None

    /**
     * Vérifie si la grille actuelle est dans l'état final (grille solution)
     *
     * @return true si la grille correspond à la grille de solution, false sinon
     */
    def isFinal: Boolean = grille == solutionGrille

    /**
     * Heuristique qui retourne le nombre de pièces mal placées par rapport à l'état final.
     *
     * @return Un entier représentant le nombre de pièces mal placées.
     */
    def H1: Int =
        grille.flatten.zipWithIndex.count {
            case (Piece.Number(value), idx) => value != idx + 1
            case (Piece.Empty, _) => false
        }

    /**
     * Heuristique qui calcule la somme des distances de Manhattan de chaque pièce à sa position finale.
     *
     * @return Un entier représentant la somme des distances de Manhattan.
     */
    def H2: Int =
        grille.flatten.zipWithIndex.map {
            case (Piece.Number(value), idx) =>
                val correctRow = (value - 1) / N
                val correctCol = (value - 1) % N
                val currentRow = idx / N
                val currentCol = idx % N
                Math.abs(correctRow - currentRow) + Math.abs(correctCol - currentCol)
            case (Piece.Empty, _) => 0
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
    //println(s"ex3.solve()                 -> ${ex3.solve()}")
    //println(s"ex3.solveHeuristique(H1) -> ${ex3.solveHeuristique(H1)}")
    //println(s"ex3.solveHeuristique(H2) -> ${ex3.solveHeuristique(H2)}")
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
