import java.util.*;
import java.io.*;
public class Cowqueue {
        public static void main(String args[]) throws Exception {
            Scanner myscan = new Scanner(new File("cowqueue.in"));
            PrintWriter writefile = new PrintWriter("cowqueue.out");
            int finaltime = 0;
            int cowamount = myscan.nextInt();
            for (int i = 0; i < cowamount; i++) {
                int a = myscan.nextInt();
                int b = myscan.nextInt();
                finaltime = Math.max(a,finaltime) + b;
            }
            writefile.print(finaltime);
            writefile.close();
            return;
        }
    
}
