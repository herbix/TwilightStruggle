package test

import org.junit.{Assert, Test}

/**
  * Created by Chaofan on 2016/9/9.
  */
class InitializationTest {
  abstract class Base {
    method()
    def method(): Unit
  }

  class Subclass extends Base {
    val obj = new Object
    override def method(): Unit = {
      Assert.assertNull(obj)
      // How to access obj here?
    }
  }

  @Test
  def doTest(): Unit = {
    new Subclass()
  }
}
