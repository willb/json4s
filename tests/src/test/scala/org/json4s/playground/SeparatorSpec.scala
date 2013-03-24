package org.json4s
package playground

import org.specs2.mutable.Specification

class SeparatorSpec extends Specification {
  
  "Bracket Separator" should {
    val sep = by.Dots
    
    "Wrap an index" in {
      sep.wrapIndex(4) must_== "[4]"
    }

    "Wrap keys correctly" in {
      sep.wrap("cats","pigs.dogs") must_== "pigs.dogs.cats"
      sep.wrap("[2].cats", "pigs.dogs") must_== "pigs.dogs[2].cats"
      sep.wrap("[2][3].cats", "pigs.dogs") must_== "pigs.dogs[2][3].cats"
      sep.wrap("cats[2]", "pigs.dogs") must_== "pigs.dogs.cats[2]"
      sep.wrap("foo","") must_== "foo"
      sep.wrap("foo","bar") must_== "bar.foo"
      sep.wrap("[0].foo","bar") must_== "bar[0].foo"
    }

    "Wrap complex keys" in {
      sep.wrap("cats.dogs", "foo.bar") must_== "foo.bar.cats.dogs"
      sep.wrap("[0].cats" , "foo.bar") must_== "foo.bar[0].cats"
      sep.wrap("[0][1].cats[2]" , "foo.bar") must_== "foo.bar[0][1].cats[2]"
    }
    
    "Append index" in {
      sep.appendIndex("key",2) must_== "key[2]"
      sep.appendIndex("foo.bar",2) must_== "foo.bar[2]"
      sep.appendIndex("foo.bar[1]",2) must_== "foo.bar[1][2]"
    }
    
    "Extract index" in {
      sep.getIndex("cats[1].dogs") must_== Some(1)
    }
    
    "Miss Index" in {
      sep.getIndex("cats") must_== None
    }
    
    "Strip first index" in {
      sep.splitAtFirstIndex("cats[5][2]") must_== ("cats","[2]")
    }

    "Strip around indexes with and without endings" in {
      sep.stripPrefix("cats.dogs[0].pigs","cats.dogs[0]") must_== "pigs"
      sep.stripPrefix("cats.dogs[0].pigs","cats") must_== "dogs[0].pigs"
      sep.stripPrefix("cats.dogs[0].pigs","cats.dogs") must_== "[0].pigs"
    }
    
    "Strip first index of missing index" in {
      sep.splitAtFirstIndex("foo.bar") must_== ("foo.bar","")
    }

    "Can strip the last index" in {
      sep.stripTailingIndex("cats.dogs[0]") must_== "cats.dogs"
    }

    "Split at first index correctly" in {
      sep.splitAtFirstIndex("cats.dogs[0].pigs") must_== ("cats.dogs",".pigs")
      sep.splitAtFirstIndex("cats[1000]") must_== ("cats","")
    }

    "stripFirst on indexes yields the identity" in {
      sep.stripFirst("[0].foo") must_== "[0].foo"
    }

    "Can detect of a path ends with an index" in {
      sep.endsWithIndex("cats.dogs[0]") must_== true
    }

    "Can collect all indexes in a key" in {
      sep.getIndexes("cats[0].dogs[1].pigs[2][3]") must_== 0::1::2::3::Nil
    }

    "Can strip index from key" in {
      sep.stripFrontIndex("[22].foo.bar") must_== "foo.bar"
      sep.stripFrontIndex("[22][23].foo.bar") must_== "[23].foo.bar"
      sep.stripFrontIndex("foo.bar") must_== "foo.bar"
    }

    "topLevelOnly works" in {
      sep.topLevelOnly("foo.bar") must_== "foo"
      sep.topLevelOnly("foo[1].bar") must_== "foo"
      sep.topLevelOnly("[1].foo.bar") must_== "[1]"
      sep.topLevelOnly("[1][2].foo.bar") must_== "[1]"
    }
  }
  
  "DoubleBracketArraySeparator" should {
    val sep = new Separator(".", "", "[[","]]") {}

    "Wrap an index" in {
      sep.wrapIndex(4) must_== "[[4]]"
    }

    "Wrap keys correctly" in {
      sep.wrap("cats","pigs.dogs") must_== "pigs.dogs.cats"
      sep.wrap("[[2]].cats", "pigs.dogs") must_== "pigs.dogs[[2]].cats"
      sep.wrap("[[2]][[3]].cats", "pigs.dogs") must_== "pigs.dogs[[2]][[3]].cats"
      sep.wrap("cats[[2]]", "pigs.dogs") must_== "pigs.dogs.cats[[2]]"
      sep.wrap("foo","") must_== "foo"
      sep.wrap("foo","bar") must_== "bar.foo"
      sep.wrap("[[0]].foo","bar") must_== "bar[[0]].foo"
    }

    "Wrap complex keys" in {
      sep.wrap("cats.dogs", "foo.bar") must_== "foo.bar.cats.dogs"
      sep.wrap("[[0]].cats" , "foo.bar") must_== "foo.bar[[0]].cats"
      sep.wrap("[[0]][[1]].cats[[2]]" , "foo.bar") must_== "foo.bar[[0]][[1]].cats[[2]]"
    }

    "Append index" in {
      sep.appendIndex("key",2) must_== "key[[2]]"
      sep.appendIndex("foo.bar",2) must_== "foo.bar[[2]]"
      sep.appendIndex("foo.bar[[1]]",2) must_== "foo.bar[[1]][[2]]"
    }

    "Extract index" in {
      sep.getIndex("cats[[1]].dogs") must_== Some(1)
    }

    "Miss Index" in {
      sep.getIndex("cats") must_== None
    }

    "Strip first index" in {
      sep.splitAtFirstIndex("cats[[5]][[2]]") must_== ("cats","[[2]]")
    }

    "Strip around indexes with and without endings" in {
      sep.stripPrefix("cats.dogs[[0]].pigs","cats.dogs[[0]]") must_== "pigs"
      sep.stripPrefix("cats.dogs[[0]].pigs","cats") must_== "dogs[[0]].pigs"
      sep.stripPrefix("cats.dogs[[0]].pigs","cats.dogs") must_== "[[0]].pigs"
    }

    "Strip first index of missing index" in {
      sep.splitAtFirstIndex("foo.bar") must_== ("foo.bar","")
    }

    "Can strip the last index" in {
      sep.stripTailingIndex("cats.dogs[[0]]") must_== "cats.dogs"
    }

    "Split at first index correctly" in {
      sep.splitAtFirstIndex("cats.dogs[[0]].pigs") must_== ("cats.dogs",".pigs")
      sep.splitAtFirstIndex("cats[[1000]]") must_== ("cats","")
    }

    "stripFirst on indexes yields the identity" in {
      sep.stripFirst("[[0]].foo") must_== "[[0]].foo"
    }

    "Can detect of a path ends with an index" in {
      sep.endsWithIndex("cats.dogs[[0]]") must_== true
    }

    "Can collect all indexes in a key" in {
      sep.getIndexes("cats[[0]].dogs[[1]].pigs[[2]][[3]]") must_== 0::1::2::3::Nil
    }

    "Can strip index from key" in {
      sep.stripFrontIndex("[[22]].foo.bar") must_== "foo.bar"
      sep.stripFrontIndex("[[22]][[23]].foo.bar") must_== "[[23]].foo.bar"
      sep.stripFrontIndex("foo.bar") must_== "foo.bar"
    }

    "topLevelOnly works" in {
      sep.topLevelOnly("foo.bar") must_== "foo"
      sep.topLevelOnly("foo[[1]].bar") must_== "foo"
      sep.topLevelOnly("[[1]].foo.bar") must_== "[[1]]"
      sep.topLevelOnly("[[1]][[2]].foo.bar") must_== "[[1]]"
    }
  }
  
  "LeftBracketArraySeparator" should {
    val sep = new Separator(".", "", "[", "") {}
    
    "Wrap index" in {
      sep.wrapIndex(4) must_== "[4"
    }

    "Wrap keys correctly" in {
      sep.wrap("cats","pigs.dogs") must_== "pigs.dogs.cats"
      sep.wrap("[2.cats", "pigs.dogs") must_== "pigs.dogs[2.cats"
      sep.wrap("[2[3.cats", "pigs.dogs") must_== "pigs.dogs[2[3.cats"
      sep.wrap("cats[2", "pigs.dogs") must_== "pigs.dogs.cats[2"
      sep.wrap("foo","") must_== "foo"
      sep.wrap("foo","bar") must_== "bar.foo"
      sep.wrap("[0.foo","bar") must_== "bar[0.foo"
    }

    "Wrap complex keys" in {
      sep.wrap("cats.dogs", "foo.bar") must_== "foo.bar.cats.dogs"
      sep.wrap("[0.cats" , "foo.bar") must_== "foo.bar[0.cats"
      sep.wrap("[0[1.cats[2" , "foo.bar") must_== "foo.bar[0[1.cats[2"
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

    "Split at first index for single index" in {
      sep.splitAtFirstIndex("cats[3.dogs") must_== ("cats", ".dogs")
    }

    "Strip first index from multiple indexes" in {
      sep.splitAtFirstIndex("cats[5[2") must_== ("cats","[2")
    }

    "Strip around indexes with and without endings" in {
      sep.stripPrefix("cats.dogs[0.pigs","cats.dogs[0") must_== "pigs"
      sep.stripPrefix("cats.dogs[0.pigs","cats") must_== "dogs[0.pigs"
      sep.stripPrefix("cats.dogs[0.pigs","cats.dogs") must_== "[0.pigs"
    }
    
    "Strip first index of missing index" in {
      sep.splitAtFirstIndex("cats") must_== ("cats","")
    }

    "Can strip the last index" in {
      sep.stripTailingIndex("cats.dogs[0") must_== "cats.dogs"
    }

    "Split at first index correctly" in {
      sep.splitAtFirstIndex("cats.dogs[0.pigs") must_== ("cats.dogs",".pigs")
      sep.splitAtFirstIndex("cats[1000") must_== ("cats","")
    }

    "stripFirst on indexes yields the identity" in {
      sep.stripFirst("[0.foo") must_== "[0.foo"
    }

    "Can detect of a path ends with an index" in {
      sep.endsWithIndex("cats.dogs[0") must_== true
    }

    "Can collect all indexes in a key" in {
      sep.getIndexes("cats[0.dogs[1.pigs[2[3") must_== 0::1::2::3::Nil
    }

    "Can strip index from key" in {
      sep.stripFrontIndex("[22.foo.bar") must_== "foo.bar"
      sep.stripFrontIndex("[22[23.foo.bar") must_== "[23.foo.bar"
      sep.stripFrontIndex("foo.bar") must_== "foo.bar"
    }

    "topLevelOnly works" in {
      sep.topLevelOnly("foo.bar") must_== "foo"
      sep.topLevelOnly("foo[1.bar") must_== "foo"
      sep.topLevelOnly("[1.foo.bar") must_== "[1"
      sep.topLevelOnly("[1[2.foo.bar") must_== "[1"
    }
  }

  "Square and Bracket Separator" should {
    val sep = new Separator("[", "]", "(", ")") {}

    "Wrap index" in {
      sep.wrapIndex(4) must_== "(4)"
    }

    "Wrap keys correctly" in {
      sep.wrap("cats","pigs[dogs]") must_== "pigs[dogs][cats]"
      sep.wrap("(2)[cats]", "pigs[dogs]") must_== "pigs[dogs(2)][cats]"
      sep.wrap("(2)(3)[cats]", "pigs[dogs]") must_== "pigs[dogs(2)(3)][cats]"
      sep.wrap("cats(2)", "pigs[dogs]") must_== "pigs[dogs][cats(2)]"
      sep.wrap("foo","") must_== "foo"
      sep.wrap("foo","bar") must_== "bar[foo]"
      sep.wrap("(0)[foo]","bar") must_== "bar(0)[foo]"
    }

    "Wrap complex keys" in {
      sep.wrap("cats[dogs]", "foo[bar]") must_== "foo[bar][cats][dogs]"
      sep.wrap("(0)[cats]" , "foo[bar]") must_== "foo[bar(0)][cats]"
      sep.wrap("(0)(1)[cats(2)]" , "foo[bar]") must_== "foo[bar(0)(1)][cats(2)]"
    }

    "Append index" in {
      sep.appendIndex("foo",2) must_== "foo(2)"
      sep.appendIndex("foo[bar]",2) must_== "foo[bar(2)]"
    }

    "Extract index" in {
      sep.getIndex("cats(1)") must_== Some(1)
    }

    "Miss Index" in {
      sep.getIndex("cats") must_== None
    }

    "Split at first index for single index" in {
      sep.splitAtFirstIndex("cats(3)[dogs]") must_== ("cats", "[dogs]")
    }

    "Strip first index from multiple indexes" in {
      sep.splitAtFirstIndex("cats(5)(2)") must_== ("cats","(2)")
    }

    "Strip around indexes with and without endings" in {
      sep.stripPrefix("cats[dogs(0)][pigs]","cats[dogs(0)]") must_== "pigs"
      sep.stripPrefix("cats[dogs(0)][pigs]","cats") must_== "dogs(0)[pigs]"
      sep.stripPrefix("cats[dogs(0)][pigs]","cats[dogs]") must_== "(0)[pigs]"
    }

    "Strip first index of missing index" in {
      sep.splitAtFirstIndex("cats") must_== ("cats","")
    }

    "Can strip the last index" in {
      sep.stripTailingIndex("cats[dogs(0)]") must_== "cats[dogs]"
    }

    "Split at first index correctly" in {
      sep.splitAtFirstIndex("cats[dogs(0)][pigs]") must_== ("cats[dogs","][pigs]")
      sep.splitAtFirstIndex("[cats(1000)]") must_== ("[cats","]")
    }

    "stripFirst on indexes yields the identity" in {
      sep.stripFirst("(0)[foo]") must_== "(0)[foo]"
    }

    "Can detect of a path ends with an index" in {
      sep.endsWithIndex("cats[dogs(0)]") must_== true
    }

    "Can collect all indexes in a key" in {
      sep.getIndexes("cats(0)[dogs(1)][pigs(2)(3)]") must_== 0::1::2::3::Nil
    }

    "Can strip index from key" in {
      sep.stripFrontIndex("(22)[foo][bar]") must_== "foo[bar]"
      sep.stripFrontIndex("(22)(23)[foo][bar]") must_== "(23)[foo][bar]"
      sep.stripFrontIndex("foo[bar]") must_== "foo[bar]"
    }

    "topLevelOnly works" in {
      sep.topLevelOnly("foo[bar]") must_== "foo"
      sep.topLevelOnly("foo(1)[bar]") must_== "foo"
      sep.topLevelOnly("(1)[foo][bar]") must_== "(1)"
      sep.topLevelOnly("(1)(2)[foo][bar]") must_== "(1)"
    }
  }
  
}