import java.util.*;
import java.io.*;
class BucketList {
    public static void main(String args[]) throws Exception {
        Scanner myscan = new Scanner(new File("blist.in"));
		PrintWriter writefile = new PrintWriter("blist.out");
        int cowamount = myscan.nextInt();
        int[] timeamount = new int[1000];
        for (int cownum = 0;cownum < cowamount;cownum++) {
            int start = myscan.nextInt();
			int end = myscan.nextInt();
			int amount = myscan.nextInt();
			for(int i = start; i < end; i++)
			{
				timeamount[i] += amount;
			}
        }
        int maxbuckets = 0;
        for (int i = 0; i < 1000;i++) {
            if (maxbuckets < timeamount[i]) {maxbuckets = timeamount[i];}
        }
        
		writefile.print(maxbuckets);
		writefile.close();
        return;
    }

}