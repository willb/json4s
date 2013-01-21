package playground

import org.specs2.mutable.Specification

class ArraySeparatorSpec extends Specification {
  
  "BraketArraySeparator" should {
    val sep = squareBraketArraySeparator
    
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
  
  "doubleBraketArraySeparator" should {
    val sep = new ArraySeparator("[[","]]"){}
    
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
  }
  
  "halfSquareBraketArraySeparator" should {
    val sep = new ArraySeparator("[",""){}
    
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
  }
  
  "doubleHalfBraketArraySeparator" should {
    val sep = new ArraySeparator("[[","") {}
    
    "Append index" in {
      sep.appendIndex("key",2) must_== "key[[2"
    }
    
    "Extract index" in {
      sep.getIndex("cats[[1") must_== Some(1)
    }
    
    "Miss Index" in {
      sep.getIndex("cats") must_== None
    }
    
    "Strip first index" in {
      sep.stripFirstIndex("cats[[5[[2") must_== ("cats","[[2")
    }
  }
}