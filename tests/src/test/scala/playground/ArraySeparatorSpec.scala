package playground

import org.specs2.mutable.Specification

class ArraySeparatorSpec extends Specification {
  
  "BraketArraySeparator" should {
    val sep = squareBraketArraySeparator
    
    "Wrap index" in {
      sep.wrapIndex(4) must_== "[4]"
    }
    
    "Append index" in {
      sep.appendIndex("key",2) must_== "key[2]"
    }
    
    "Extract index" in {
      sep.getIndex("cats[1]") must_== Some(1)
    }
    
    "Miss Index" in {
      sep.getIndex("cats") must_== None
    }
    
    "Strip first index" in {
      sep.stripFirstIndex("cats[5][2]") must_== ("cats","[2]")
    }
    
    "Strip first index of missing index" in {
      sep.stripFirstIndex("cats") must_== ("cats","")
    }
    
  }
  
  "DoubleBraketArraySeparator" should {
    val sep = new ArraySeparator("[[","]]") {}
    
    "Wrap index" in {
      sep.wrapIndex(4) must_== "[[4]]"
    }
    
    "Append index" in {
      sep.appendIndex("key",2) must_== "key[[2]]"
    }
    
    "Extract index" in {
      sep.getIndex("cats[[1]]") must_== Some(1)
    }
    
    "Miss Index" in {
      sep.getIndex("cats") must_== None
    }
    
    "Strip first index" in {
      sep.stripFirstIndex("cats[[5]][[2]]") must_== ("cats","[[2]]")
    }
    
    "Strip first index of missing index" in {
      sep.stripFirstIndex("cats") must_== ("cats","")
    }
    
  }
  
  "LeftBraketArraySeparator" should {
    val sep = new ArraySeparator("[","") {}
    
    "Wrap index" in {
      sep.wrapIndex(4) must_== "[4"
    }
    
    "Append index" in {
      sep.appendIndex("key",2) must_== "key[2"
    }
    
    "Extract index" in {
      sep.getIndex("cats[1") must_== Some(1)
    }
    
    "Miss Index" in {
      sep.getIndex("cats") must_== None
    }
    
    "Strip first index" in {
      sep.stripFirstIndex("cats[5[2") must_== ("cats","[2")
    }
    
    "Strip first index of missing index" in {
      sep.stripFirstIndex("cats") must_== ("cats","")
    }
  }
  
  "CrazyBraketArraySeparator" should {
    val sep = new ArraySeparator("({[","$$%^") {}
    
    "Wrap index" in {
      sep.wrapIndex(4) must_== "({[4$$%^"
    }
    
    "Append index" in {
      sep.appendIndex("key",2) must_== "key({[2$$%^"
    }
    
    "Extract index" in {
      sep.getIndex("cats({[1$$%^") must_== Some(1)
    }
    
    "Miss Index" in {
      sep.getIndex("cats") must_== None
    }
    
    "Strip first index" in {
      sep.stripFirstIndex("cats({[5$$%^({[2$$%^") must_== ("cats","({[2$$%^")
    }
    
    "Strip first index of missing index" in {
      sep.stripFirstIndex("cats") must_== ("cats","")
    }
  }
  
}