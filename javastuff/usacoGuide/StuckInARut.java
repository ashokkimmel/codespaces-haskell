import java.util.*;
import java.io.*;
class BucketList {
    public static void main(String args[]) throws Exception {
        Scanner myscan = new Scanner(new File("blist.in"));
		PrintWriter writefile = new PrintWriter("blist.out");
        int n = myscan.nextInt();
        String cowdirs [][] = new int[n];
        int xs [][] = new int[n];
        int ys [][] = new int[n];
        ArrayList<Integer> eastcows = new Arraylist<>();
        ArrayList<Integer> northcows = new Arraylist<>();
        for (int i = 0; i < cows.length; i++) {
            cowdirs[i] = myscan.next();
            eastcows[i] = myscan.nextInt();
            northcows[i] = myscan.nextInt();
            cowdirs[i].stringEquals("E") ? eastcows.add(i): northcows.add(i);
        }
        northcows.sort(Comparator.comparingInt (j -> xs[j]));
        eastcows.sort(Comparator.comparingInt (j -> xs[j]));
        northcows.foreach(i -> {
            eastcows.foreach(j -> {
                if 
            });
        });
		writefile.print(maxbuckets);
		writefile.close();
        return;
    }

}


