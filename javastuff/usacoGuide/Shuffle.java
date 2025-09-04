import java.util.*;
import java.io.*;
class Shuffle {
   public static void main(String args[]) throws Exception {
       Scanner myscan = new Scanner(new File("Shuffle.in"));
		PrintWriter writefile = new PrintWriter("Shuffle.out");
      int N = myscan.nextInt();
      int[] shufflepos = new int[N];
      int[] cowpos = new int[N];
      
      for (int i = 0; i < N; i++) {
       shufflepos[i] = myscan.nextInt();
    }
    for (int i = 0; i < N; i++) {
     cowpos[i] = myscan.nextInt();
    }
    
    for (int i = 0; i < 3; i++) {
      int[] temp = new int[N];
         for (int j = 0; j < N; j++) {
         temp[j] = cowpos[shufflepos[j] - 1];
     }
     cowpos = temp; 
   }
   String mystr = "" + cowpos[0];
        for (int i = 1; i < cowpos.length; i++) {
            mystr = mystr + "\n" + cowpos[i];
        } 

		writefile.print(mystr);
		writefile.close();
        return;
    }

}