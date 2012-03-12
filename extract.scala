val source = scala.io.Source.fromFile("music.txt")
val lines = source.mkString.split("\r\n")
source.close

// Get fields, which is an sequence of lines, split into fields

val fields = for (line <- lines; if line.contains("\t1 ")) yield line.split('\t')

// Some lines have the scale name missing because it's a repeat (but a
// different variant of) the previous line. So we'll produce a bulked-up
// version of the data with the key names filled in.
// This is a sequence of pairs (scale_name, degree_string)

val bulked_fields: Seq[Pair[String,String]] = fields.scanLeft(("" -> ""))(
	    (a,b) => if (b(0) == "") (a._1+"(2)" -> b(1)) else (b(0) -> b(1))
	).tail

// Map scale to degree-strings

val scale_degree_string = bulked_fields.toMap

// Get the modifier from a string degree.
// E.g. for "#4" it is a sharp and therefore +0.5

def mod(str: String): Double = str.filterNot { _.isDigit } match {
	case ""   =>  0
	case "#"  => +0.5
	case "b"  => -0.5
	case "n"  =>  0
	case "-#" => +0.25
	case "+#" => +0.75
	case "-b" => -0.25
	case "+b" => -0.75
	case x    => throw new Exception("No mod available for '" + x + "'")
}

// A function which converts a degree string to the main tone above root.
// E.g. The main tone in "#2" is 2, which is a 1 whole tone above the root.

def tone(str: String): Double = str.filter { _.isDigit } match {
	case "1" => 0
	case "2" => 1
	case "3" => 2
	case "4" => 2.5
	case "5" => 3.5
	case "6" => 4.5
	case "7" => 5.5
	case "8" => 6
	case _   => throw new Exception("No tone for '" + str + "'")
}

def degree_to_tone(deg: String) = deg.split(' ') map { s => tone(s) + mod(s) }


// Create a mapping from scale name to its sequence of tones

val scale_tones = for {sc_deg_str <- scale_degree_string.toList sortBy { _._1 }
} yield ( sc_deg_str._1 -> degree_to_tone(sc_deg_str._2) )

// Note names for each semitone offset from C.

val note_names = List("C", "C#", "D", "D#", "E",
	"F", "F#", "G", "G#", "A", "A#", "B")
val num_notes = note_names.length

def note(tone: Double) = note_names((tone * 2).toInt % num_notes)
def degree_to_note(deg: String) = degree_to_tone(deg) map { note(_) }

def note_using_flats(note: String) = note match {
	case "C#" => "Db"
	case "D#" => "Eb"
	case "F#" => "Gb"
	case "G#" => "Ab"
	case "A#" => "Bb"
	case _    => note
}
def note_using_flats(notes: Seq[String]): Seq[String] =
	notes map { note_using_flats(_) }

// This is what makes a scale with a key

class KeyedScale(val scale: String, val tonic: String, val notes: Seq[String]) {
	def hasSharps: Boolean =
		(tonic contains '#') || (notes exists { _ contains '#'})
	def hasFlats: Boolean =
		(tonic contains 'b') || (notes exists { _ contains 'b'})
	def flatCopy = new KeyedScale(
			scale, note_using_flats(tonic), note_using_flats(notes))
}

// Output scale, tonic and notes.
// Both in pretty format, and CSV format

def pretty_format(ks: KeyedScale) =
		ks.scale + " in " + ks.tonic + ": " + ks.notes.mkString(" ")

val note_columns = List("C", "C#", "Db", "D", "D#", "Eb", "E",
	"F", "F#", "Gb", "G", "G#", "Ab", "A", "A#", "Bb", "B")

def csv_format(ks: KeyedScale) =
	ks.scale + "," +
	ks.tonic + "," +
	ks.hasSharps + "," +
	ks.hasFlats + "," +
	ks.notes.length + "," +
	(note_columns map {n => if (ks.notes contains n) n else "" }).mkString(",")

def html_format(ks: KeyedScale): String = {
	val note_classes = ks.notes map { note: String =>("note-" + note).
				replaceAllLiterally("#", "-sharp").
				replaceAllLiterally("b", "-flat").
				toLowerCase
		} mkString(" ")
	"<div class=\"" +
		(if (ks.hasSharps) "has-sharps " else "") +
		(if (ks.hasFlats) "has-flats " else "") +
		note_classes +
		"\">" + ks.scale + " in " + ks.tonic + " (" + ks.notes.length + "): " +
		ks.notes.mkString(" ") +
		"</div>"
}

// Print the notes for each scale with each tonic

println("Scale,Key,Has sharps,Has flats,Num pitch classes," +
	note_columns.mkString(","))

scale_tones.toList sortBy { _._1 } foreach { scale_tone_seq =>
	val (scale, tones) = scale_tone_seq
	(0 until num_notes).foreach { tonic_semitone_offset =>
		val tonic_tone_offset = tonic_semitone_offset.toDouble / 2
		val tonic_name = note_names(tonic_semitone_offset)
		val keyed_tones = tones map { _ + tonic_tone_offset }
		val keyed_notes = keyed_tones map { note(_) }
		val ks = new KeyedScale(scale, tonic_name, keyed_notes)
		val ks_flattened = new KeyedScale(scale,
					note_using_flats(tonic_name),
					note_using_flats(keyed_notes))
		println(html_format(ks))
		if (ks.hasSharps)
			println(html_format(ks.flatCopy))
	}
}
