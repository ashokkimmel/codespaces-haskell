import java.util.*;
import java.io.*;
class SpeedingTicket {
    public static void main(String args[]) throws Exception {
        Scanner myscan = new Scanner(new File("blist.in"));
		PrintWriter writefile = new PrintWriter("blist.out");
        int n = myscan.nextInt();
        int m = myscan.nextInt();
        int[][] roadsegs = new int[n][2];
        int[][] cowsegs = new int[m][2];
        for (int i = 0; i < n; i++) {
            roadsegs[i][0] = myscan.nextInt();
            roadsegs[i][1] = myscan.nextInt();
        }
        for (int i = 0; i < m; i++) {
            cowsegs[i][0] = myscan.nextInt();
            cowsegs[i][1] = myscan.nextInt();
        }
        int cowdist = 0;
        int roaddist = 0;
        int roadseg = 0;
        int cowseg = 0;
        int lastlimit = 0;
        int lastspeed = 0;
        int highestbreak = 0;
        while (roaddist != 0 || cowdist != 0) {
            if (cowdist == roaddist) {
                roaddist += roadsegs[roadseg][0];
                lastlimit = roadsegs[roadseg][1];
                cowdist += cowsegs[cowseg][0];
                lastspeed = cowsegs[cowseg][1];
                if (lastspeed - lastlimit > highestbreak) {highestbreak = lastspeed - lastlimit;}
            } else if (cowdist > roaddist) {
                roaddist += roadsegs[roadseg][0];
                lastlimit = roadsegs[roadseg][1];
                if (lastspeed - lastlimit > highestbreak) {highestbreak = lastspeed - lastlimit;}
            } else {
                cowdist += cowsegs[cowseg][0];
                lastspeed = cowsegs[cowseg][1];
                if (lastspeed - lastlimit > highestbreak) {highestbreak = lastspeed - lastlimit;}
            
                
            }
        }
		writefile.print(highestbreak);
		writefile.close();
        return;
    }

}