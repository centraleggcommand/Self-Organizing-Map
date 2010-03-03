import scala.io.Source
import javax.servlet.http._
import textprocessing.StopWords
import textprocessing.Stemmer
import textprocessing.InputTicket

class FilterInput extends HttpServlet
{

  override def doPost( request:HttpServletRequest,
                       response:HttpServletResponse)
  {
    val input = request.getParameter("comments")
    val inputList = List.fromString(input,' ')
    val filteredList = StopWords.removeStopwords(inputList)
    var stemmer = new Stemmer()
    val stemmedList = filteredList.map(stemmer.stemWord(_))
    val ticket = new InputTicket(stemmedList)
    // Create the output
    response.setContentType("text/html")
    val writerOut = response.getWriter()
    writerOut.println("Here is the word map content:<br/>")
    writerOut.println(ticket.getWordMap.toString)
    writerOut.flush()
  }
}

