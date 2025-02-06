package collections

object task_collections {

	def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

	/**
	 * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
	 * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
	 * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
	 * Пример:
	 * capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
	 * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
	 * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
	 * HINT: Тут удобно использовать collect и zipWithIndex
	 *
	 * * */
	def capitalizeIgnoringASCII(text: List[String]): List[String] = {
		text.zipWithIndex.collect {
			case (letter, idx) if idx == 0 => letter
			case (letter, _) => if (isASCIIString(letter)) letter.toUpperCase
			else letter.toLowerCase
		}
	}

	/**
	 *
	 * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
	 * В тексте встречаются числа от 0 до 9
	 *
	 * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
	 *
	 * HINT: Для всех возможных комбинаций чисел стоит использовать Map
	 * * */
	def numbersToNumericString(text: String): String = {
		val digitMap: Map[Char, String] = Map(
			'0' -> "zero",   // замена для 0
			'1' -> "one",    // замена для 1
			'2' -> "two",    // замена для 2
			'3' -> "three",  // замена для 3
			'4' -> "four",   // замена для 4
			'5' -> "five",   // замена для 5
			'6' -> "six",    // замена для 6
			'7' -> "seven",  // замена для 7
			'8' -> "eight",  // замена для 8
			'9' -> "nine"    // замена для 9
		)

		text.map(ch => if (ch.isDigit) digitMap(ch) else ch.toString).mkString("")
	}

	/**
	 *
	 * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
	 * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии что записи уникальные и не имеют повторений
	 * HINT: Set
	 * HINT2: Iterable стоит изменить
	 * * */

	case class Auto(mark: String, model: String)

	/**
	 * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
	 * Реализуйте метод который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
	 * */
	def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
		dealerOne.toSet ++ dealerTwo.toSet
	}

	/**
	 * Хотим узнать какие машины обслуживается в первом дилеромском центре, но не обслуживаются во втором
	 * Реализуйте метод который примет две коллекции (два источника)
	 * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
	 * */
	def filterAllLeftDealerAutoWithoutRight(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
		dealerOne.toSet -- dealerTwo.toSet
	}
}