package playground

import org.specs2.mutable.Specification

class SeparatorSpec extends Specification {
  
  "Bracket Separator" should {
    val sep = by.Dots
    
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
      sep.splitAtFirstIndex("cats[5][2]") must_== ("cats","[2]")
    }
    
    "Strip first index of missing index" in {
      sep.splitAtFirstIndex("cats") must_== ("cats","")
    }
    
  }
  
  "DoubleBracketArraySeparator" should {
    val sep = new Separator(".", "", "[[","]]") {}
    
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
      sep.splitAtFirstIndex("cats[[5]][[2]]") must_== ("cats","[[2]]")
    }
    
    "Strip first index of missing index" in {
      sep.splitAtFirstIndex("cats") must_== ("cats","")
    }
    
  }
  
  "LeftBracketArraySeparator" should {
    val sep = new Separator(".", "", "[", "") {}
    
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
      sep.splitAtFirstIndex("cats[5[2") must_== ("cats","[2")
    }
    
    "Strip first index of missing index" in {
      sep.splitAtFirstIndex("cats") must_== ("cats","")
    }
  }
  
  "CrazyBracketArraySeparator" should {
    val sep = new Separator(".", "", "({[","$$%^") {}
    
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
      sep.splitAtFirstIndex("cats({[5$$%^({[2$$%^") must_== ("cats","({[2$$%^")
    }
    
    "Strip first index of missing index" in {
      sep.splitAtFirstIndex("cats") must_== ("cats","")
    }
  }

  "Square and Bracket Separator" should {
    val sep = new Separator("[", "]", "(", ")") {}
    val catIndexPath = "cats(0)[dogs][pigs]"
    "Strip first index" in {
      sep.stripPrefix(catIndexPath,"cats") must_== "0[dogs][pigs]"
      sep.stripPrefix(catIndexPath,"cats(0)") must_== "dogs[pigs]"
    }

    val dogIndexPath = "cats[dogs(0)][pigs]"

    "Strip around indexes with endings" in {
      sep.stripPrefix(dogIndexPath,"cats[dogs(0)]") must_== "pigs"
      sep.stripPrefix(dogIndexPath,"cats[dogs]") must_== "0[pigs]"
    }

    "Strip around indexes without endings" in {
      by.Dots.stripPrefix("cats.dogs[0].pigs", "cats.dogs[0]") must_== "pigs"
      by.Dots.stripPrefix("cats.dogs[0].pigs", "cats.dogs") must_== "0.pigs"
    }

    "Can strip the last index" in {
      sep.stripTailingIndex("cats[dogs(0)]") must_== "cats[dogs]"
    }

    "Cast strip first indexes" in {
      sep.stripFirst("(0)[foo]") must_== "0[foo]"
    }

    "Can detect of a path ends with an index" in {
      sep.endsWithIndex("cats[dogs(0)]") must_== true
    }
  }
  
}