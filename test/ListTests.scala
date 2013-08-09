import org.scalatest.FunSuite

class ListTests extends FunSuite {

  test("length of a list") {
    assert (P04.length(List(1, 2, 3)) == 3)
  }
  
  test("length of Nil") {
    assert (P04.length(Nil) == 0)
  }
  
  
}