import scala.io.StdIn
/**
 * Enumération représentant une pièce du taquin
 * - Trou représente la case vide/le trou
 * - Nombre(valeur) représente une case avec une valeur qui est un nombre naturel
 */
enum Piece:
    case Trou
    case Nombre(valeur: Int)

/**
 * Fonction qui convertit une chaîne de caractères en une grille de Taquin
 *
 * @param input Une chaîne de caractères représentant une grille de Taquin sous forme de tableau
 *              La case vide est représentée par un "_"
 * @return Un Either qui est un Right ayant une liste de listes de pièces, représentant la grille du Taquin
 *         ou un Left avec une String contenant une erreur si la chaîne d'entrée ne respecte pas le format attendu 
 *                                  ou s'il n'y a pas exactement une case vide
 */
def parseTaquin(input: String): Either[String, List[List[Piece]]] =
    val cleanedInput = input.replaceAll("\\s+", " ").replaceAll("\\[ ", "[").replaceAll(" \\]", "]").trim

    /**
     * Fonction interne qui analyse une ligne de la grille de Taquin et la convertit en une liste de pièces (Piece)
     *
     * @param row Une chaîne de caractères représentant une ligne de la grille, avec des nombres séparés par des espaces 
     *            et "_" représentant la case vide
     * @return Un Either est un Right ayant liste de pièces,
     *          ou un Left avec une String contenant une erreur
     */
    def parseRow(row: String): Either[String, List[Piece]] =
        row.split(" ").toList.foldRight(Right(Nil): Either[String, List[Piece]])((e, acc) =>
            val parsedPiece = e match
                case "_" => Right(Piece.Trou)
                case nombre =>
                    try Right(Piece.Nombre(nombre.toInt))
                    catch case _: NumberFormatException => Left(s"Format de Nombre invalide pour la valeur : $nombre")
            parsedPiece.flatMap(piece => acc.map(piece :: _))
        )

    if (!(cleanedInput.startsWith("[[") && cleanedInput.endsWith("]]")))
        Left("Format invalide : l'entrée doit commencer par '[[' et finir par ']]'")
    else
        val underscoreCount = input.count(_ == '_')
        if (underscoreCount != 1)
            Left("Format invalide : l'entrée doit contenir exactement un '_' représentant la case vide (Trou)")
        else
            val gridRowsRaw = cleanedInput.stripPrefix("[[").stripSuffix("]]").split("\\]\\[").toList
            gridRowsRaw.foldRight(Right(Nil): Either[String, List[List[Piece]]])((row, acc) =>
                parseRow(row).flatMap(parsedRow => acc.map(parsedRow :: _))
            ).flatMap(gridRows =>
                val N = gridRows.size
                val M = gridRows.headOption.map(_.size).getOrElse(0)
                if (!gridRows.forall(_.size == M))
                    Left("Format invalide : l'entrée doit représenter une grille rectangulaire (NxM)")
                else
                    val Nombres = gridRows.flatten.collect(piece => piece match
                                        case Piece.Nombre(valeur) => valeur)
                    val expectedNombres = (1 to (N * M - 1)).toSet
                    val actualNombres = Nombres.toSet
                    if (actualNombres != expectedNombres)
                        Left(s"Format invalide : l'entrée doit contenir des Nombres uniques de 1 à ${N * M - 1}")
                    else
                        Right(gridRows)
            )

/**
 * Classe représentant l'état du Taquin sous forme d'une grille
 *
 * @param grille La grille actuelle du Taquin sous forme de liste de listes de Piece
 */
case class Taquin(grille: List[List[Piece]]):
    val sigma = Set("u", "d", "l", "r")
    val N = grille.size
    val M = grille(0).size

    lazy val etatFinal = Taquin((1 until N * M).foldRight(List(Piece.Trou: Piece))((n, acc) => Piece.Nombre(n) :: acc).grouped(M).toList)

    /**
     * Affiche la grille du Taquin sous une forme lisible
     *
     * @return Une représentation en chaîne de caractères de la grille où Trou est représenté par "_"
     */
    override def toString: String = grille.map(row =>
        "[" + row.map(_ match
            case Piece.Trou => "_"
            case Piece.Nombre(n) => n.toString
        ).mkString(" ") + "]"
    ).mkString("[", "", "]")

    /**
     * Trouve la position de la case vide dans la grille
     *
     * @return Un Option du tuple (row, col) représentant les coordonnées de la 1e case vide dans la grille
     */
    def findTrou: Option[(Int, Int)] =
    grille.zipWithIndex.flatMap(rowWithIndex =>
        rowWithIndex._1.zipWithIndex.collect(pieceWithIndex => pieceWithIndex match
                case (Piece.Trou, colIndex) => (rowWithIndex._2, colIndex)
        )
    ).headOption

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
                case "r" if trouCol < M - 1 => Some(Taquin(swapTrou(trouRow, trouCol + 1)))
                case _ => None
        else
            None

    /**
     * Heuristique qui retourne le nombre de pièces mal placées par rapport à l'état final.
     *
     * @return Un entier représentant le nombre de pièces mal placées.
     */
    def H1: Int =
        grille.flatten.zipWithIndex.count(_ match
            case (Piece.Nombre(valeur), idx) => valeur != idx + 1
            case (Piece.Trou, _) => false
        )

    /**
     * Heuristique qui calcule la somme des distances de Manhattan de chaque pièce à sa position finale.
     *
     * @return Un entier représentant la somme des distances de Manhattan.
     */
    def H2: Int =
        grille.flatten.zipWithIndex.map(_ match
            case (Piece.Nombre(valeur), idx) =>
                val correctRow = (valeur - 1) / M
                val correctCol = (valeur - 1) % M
                val currentRow = idx / M
                val currentCol = idx % M
                Math.abs(correctRow - currentRow) + Math.abs(correctCol - currentCol)
            case (Piece.Trou, _) => 0
        ).sum

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

    val taquin1 = Taquin(parseTaquin("[[ _ 2][1 3]]").getOrElse(Nil))
    val ex1 = new AFD(taquin1.sigma, delta, taquin1, Set(taquin1.etatFinal))
    println(s"Tests pour le taquin : ${taquin1}")
    println(s"ex1.solve() -> ${ex1.solve()}")

    val mot1: Mot[String] = List("d", "u", "d", "r")
    println(s"ex1.accept($mot1) -> ${ex1.accept(mot1)}")

    val taquin2 = Taquin(parseTaquin("[[2 1][3 _]]").getOrElse(Nil))
    val ex2 = new AFD(taquin2.sigma, delta, taquin2, Set(taquin2.etatFinal))
    println(s"Tests pour le taquin : ${taquin2}")
    println(s"ex2.solve() -> ${ex2.solve()}")

    val taquin3 = Taquin(parseTaquin("[[2 3 6][1 _ 5][7 8 4]]").getOrElse(Nil))
    val ex3 = new AFD(taquin3.sigma, delta, taquin3, Set(taquin3.etatFinal))
    println(s"Tests pour le taquin : ${taquin3}")
    val lazySolution3H1 = ex3.lazysolve(taquin => taquin.H1)
    println(s"ex3.lazysolve(taquin => taquin.H1).take(1).toList -> ${lazySolution3H1.take(1).toList}")

    val mot2: Mot[String] = List(
            "r", "u", "l", "d", "l", "u", "r", "d", "l", "u", "r", "r", "d", "l", "u", "r", "d", "l", "u", "r", "d", "d", "l", "u", "r", "d", "l", "u", "r", "d", 
            "l", "l", "u", "r", "d", "l", "u", "r", "d", "r", "u", "l", "l", "d", "r", "r", "u", "l", "l", "d", "r", "r", "u", "l", "d", "r", "u", "l", "d", "l", "u",
            "r", "r", "d", "l", "u", "r", "d", "l", "u", "l", "d", "r", "u", "r", "d", "l", "u", "r", "d", "l", "l", "u", "r", "r", "d", "l", "l", "u", "r", "d", "l", "u", "r", "d", "r"
        )
    println(s"ex3.accept($mot2) -> ${ex3.accept(mot2)}")

    val taquin4 = Taquin(parseTaquin("[[1 2 3][4 _ 5]]").getOrElse(Nil))
    val ex4 = new AFD(taquin4.sigma, delta, taquin4, Set(taquin4.etatFinal))
    println(s"Tests pour le taquin rectangulaire : ${taquin4}")
    val lazySolution4H2 = ex4.lazysolve(taquin => taquin.H2)
    println(s"ex4.lazysolve(taquin => taquin.H2).take(1).toList -> ${lazySolution4H2.take(1).toList}")

    val startTimeH1_1 = System.nanoTime
    val lazySolutionH1_1 = ex3.lazysolve(taquin => taquin.H1)
    val solutionH1_1 = lazySolutionH1_1.take(1).toList
    val endTimeH1_1 = System.nanoTime
    println(s"Temps de résolution avec H1 pour ${taquin3} afin d'avoir 1 solutions : ${(endTimeH1_1 - startTimeH1_1) / 1000000} ms")

    val startTimeH2_1 = System.nanoTime
    val lazySolutionH2_1 = ex3.lazysolve(taquin => taquin.H2)
    val solutionH2_1 = lazySolutionH2_1.take(1).toList
    val endTimeH2_1 = System.nanoTime
    println(s"Temps de résolution avec H2 pour ${taquin3} afin d'avoir 1 solutions : ${(endTimeH2_1 - startTimeH2_1) / 1000000} ms")

    val startTimeH1_2 = System.nanoTime
    val lazySolutionH1_2 = ex3.lazysolve(taquin => taquin.H1)
    val solutionH1_2 = lazySolutionH1_2.take(5).toList
    val endTimeH1_2 = System.nanoTime
    println(s"Temps de résolution avec H1 pour ${taquin3} afin d'avoir 5 solutions : ${(endTimeH1_2 - startTimeH1_2) / 1000000} ms")

    val startTimeH2_2 = System.nanoTime
    val lazySolutionH2_2 = ex3.lazysolve(taquin => taquin.H2)
    val solutionH2_2 = lazySolutionH2_2.take(5).toList
    val endTimeH2_2 = System.nanoTime
    println(s"Temps de résolution avec H2 pour ${taquin3} afin d'avoir 5 solutions : ${(endTimeH2_2 - startTimeH2_2) / 1000000} ms")

    val startTimeH1_3 = System.nanoTime
    val lazySolutionH1_3 = ex3.lazysolve(taquin => taquin.H1)
    val solutionH1_3 = lazySolutionH1_3.take(10).toList
    val endTimeH1_3 = System.nanoTime
    println(s"Temps de résolution avec H1 pour ${taquin3} afin d'avoir 10 solutions : ${(endTimeH1_3 - startTimeH1_3) / 1000000} ms")

    val startTimeH2_3 = System.nanoTime
    val lazySolutionH2_3 = ex3.lazysolve(taquin => taquin.H2)
    val solutionH2_3 = lazySolutionH2_3.take(10).toList
    val endTimeH2_3 = System.nanoTime
    println(s"Temps de résolution avec H2 pour ${taquin3} afin d'avoir 10 solutions : ${(endTimeH2_3 - startTimeH2_3) / 1000000} ms")

    println(s"Veuillez entrer une suite de u, d, l et r (tous les caractères accolés) pour déterminer si c'est une solution du taquin ${taquin3}:")
    println(s"L'entrée est acceptée -> ${ex3.parseAccept(StdIn.readLine(), (x: String) => x.split("").toList)}")
