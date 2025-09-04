import java.util.*;
import java.io.*;
class BucketList {
    public static void main(String args[]) throws Exception {
        Scanner myscan = new Scanner(new File("blist.in"));
		PrintWriter writefile = new PrintWriter("blist.out");
        
        int N = myscan.nextInt();
        Int maxtime;
        int [][] mypoints = new int[100][2];
        for (int i = 0; i < mybuckets.length; i++) {
            mypoints[i][0] = myscan.nextInt();
            mypoints[i][1] = myscan.nextInt();
        }

		writefile.print(maxbuckets);
		writefile.close();
        return;
    }

}