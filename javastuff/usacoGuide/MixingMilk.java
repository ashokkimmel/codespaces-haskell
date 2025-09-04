import java.util.*;
import java.io.*;
class BucketList {
    public static void main(String args[]) throws Exception {
        Scanner myscan = new Scanner(new File("mixmilk.in"));
		PrintWriter writefile = new PrintWriter("mixmilk.out");
        boolean same = false;
        int[] starter = new int[3];
        int[] capacities = new int[3];
        int[] starter1 = new int[3];
        int[] temp;
        for (int i = 0; i < 3; i++) {
            starter[i]=myscan.nextInt();
            capacities[i]=myscan.nextInt();
        }
        
        
		writefile.print(maxbuckets);
		writefile.close();
        return;
    }
    public static int[] move(int[] old,int[] capacities, int movenum) {
        int newbucket = (movenum + 1) % 3; 
        return null;
    }
}