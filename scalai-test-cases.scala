  def check1(v: String): String
//==>
  def check1(
      v: String
  ): String
//===================================================
  def check2( v : String): String
//==>
  def check2(
      v: String
  ): String
//===================================================
  def check3(v: String)
//==>
  def check3(
      v: String
  )
//===================================================
  def check4(v: String)
//==>
  def check4(
      v: String
  )
//===================================================
  def check5(v: String,    a: Map[String, Any])
//==>
  def check5(
      v: String,
      a: Map[String, Any]
  )
//===================================================
  def check5(v: String, a:    Map[String, Any],   b: Map[Any, _])
//==>
  def check5(
      v: String,
      a: Map[String, Any],
      b: Map[Any, _]
  )
//===================================================
  def check6(    v: String, a:    Seq[(String, Any)],   b: Map[Any, _])
//==>
  def check6(
      v: String,
      a: Seq[(String, Any)],
      b: Map[Any, _]
  )
//===================================================
  case class Person(v: String,     args: Map[String, Seq[(String, Any)]])
//==>
  case class Person(
      v: String,
      args: Map[String, Seq[(String, Any)]]
  )
//===================================================
  class Person(v: String,     args: Map[String, Seq[(String, Any)]])
//==>
  class Person(
      v: String,
      args: Map[String, Seq[(String, Any)]]
  )
