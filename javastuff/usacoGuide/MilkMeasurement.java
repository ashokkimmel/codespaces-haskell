import java.util.*;
import java.io.*;
class MilkMeasurement {
    public static void main(String args[]) throws Exception {
        Scanner myscan = new Scanner(new File("blist.in"));
		PrintWriter writefile = new PrintWriter("blist.out");
        int n = myscan.nextInt();
     int[][] logs = new int[n][3];
       for (int i = 0;i < n; i++){
            logs[i][0] = myscan.nextInt();
           String cowname = myscan.next();
           switch(cowname.charAt(0)) {
				case 'B':
					break;
				case 'E':
					logs[i][1] = 1;
					break;
				case 'M':
					logs[i][1] = 2;
					break;
			}
            logs[i][2] = myscan.nextInt();
        }
		Arrays.sort(logs, (a, b) -> Integer.compare(a[0], b[0]));
       int[] cowamounts = {7,7,7};
        int highestmilk = 7;
        for (int i = 0;i < n;i++) {
            boolean alreadychanged = false;
            if (cowamounts[logs[i][1]] == highestmilk) {
                if (logs[i][2] > 0) {
                    highestmilk += logs[i][2];
                    cowamounts[logs[i][1]] = highestmilk;
                } else {
                    
                }
            }
        }
        writefile.print(highestmilk);
		writefile.close();
        return;
    }

}