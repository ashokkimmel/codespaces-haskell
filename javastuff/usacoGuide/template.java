import java.util.*;
import java.io.*;
class BucketList {
    public static void main(String args[]) throws Exception {
        Scanner myscan = new Scanner(new File("blist.in"));
		PrintWriter writefile = new PrintWriter("blist.out");

		writefile.print(maxbuckets);
		writefile.close();
        return;
    }

}