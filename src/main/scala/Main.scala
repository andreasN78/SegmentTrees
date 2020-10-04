import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {//kati san void
    // Create a Segment tree with a default value of 2 Using a collection. The tree aggregates the products of it's elements (see agg function).
    val constant = Seq.fill(50)(2)//ftiaxnei ena seq 50 stoixeiwn opou ola ta stoixeia to 2
    val mulSegTree = new SegmentTree[Int](1, 50, constant, (a, b) => a * b)//einai tis skala inline function
//o prwtos tropos einai to evros 1 me 50
    // What's the product of all elements between 2 and 5?
     println("QUERY time product!!") 
    var q = time{mulSegTree.query(2, 5)}
    
    println(f"The product from elements 2 to 5 is: ${q}\n")

//exoume 2 constructor mas epitrepei na orizoume 2 fores to ti theloume na kanoume
//gia na mporw na exw dynamiko euros me ton prwto tropo enw o deuteros einai static
    // OR create a segment tree from an array. The tree aggregates that sum of it's elements (see agg function)
    val values = Seq.fill(15)(Random.nextInt)
    val sumSegTree = new SegmentTree[Int](values, (a, b) => a+b)
    visualize(sumSegTree,"test1")

    // Update the element of the Segment tree at index 2. The tree updates the aggregated node values to correctly contain the new sum.
     println("UPDATE of SUM elements time!!") 
    time{sumSegTree.update(2, -1)}
    
    visualize(sumSegTree,"test2")
    
 println("\n") 
  println("SUM of given range  time ") 
    // What's the sum of all elements between 44 and 97?
    q = time{sumSegTree.query(44, 97)}//i query pairnei ena evros
    println(f"The sum from elements 44 to 97 is: ${q} \n")


    // OR create a segment tree from an array, but with custom segment indices.
    val values2 = (1 to 21).toSeq//dimiourgw ena seq me 1 ews 21 aithmous
    val minSegTree = new SegmentTree[Int](-20,0, values2, scala.math.min)
    visualize(minSegTree, "test3")//optikopoiisi kai dimiourgia onomatos eikonas

    // Whats the minimum value within the whole tree?
     println("Minimum Value range time \n") 
    q = time{minSegTree.query(-20, 0)}
    println(f"The minimum value from elements -20 to 0 is: ${q}\n")

    // Update to include an even lower value
    println("UPDATE time") 
   time{minSegTree.update(-9, -100)}
    println(f"The updated -9 to value -100\n")
 println("Minimum Value time\n") 
    // What is the minimum value now?
    q = time{minSegTree.query(-20, 0)}
    println(f"The minimum value from elements -20 to 0 is now: ${q}\n")
  }
  
  //Giannaki edw!!

  def visualize[T] (tree: SegmentTree[T], filename: String): Unit = {// kalei to string
    import java.io._
    import sys.process._

    val bw = new BufferedWriter(new FileWriter(filename))//anoixame reuma esiodou grapsame sto filename
    bw.write(tree.toGraphvizString)
    bw.close()//paragw to test

    Process(f"dot -Tpng -O ${filename}").!  //einai gia to bash to dot san na kanw ls paraxe ena png me onoma filename
    Process(f"xdg-open ${filename}.png ").run //leitourgiko sistima anoixe m ena parathiro gia na dw tn eikonas
 //gia linux mono trexei oxi gia windows
//sto paradeigma afou i prwti ti timi einai to 1 exoun ola 1
//twra pou einai i elaxisti anevainei i elaxisti pros ta panw
  }
  
  //Giannaki edw i time!!!
  
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
}


}
