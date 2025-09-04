import java.util.*;
import java.io.*;
import java.math.*;
class SleepingInClass {
    public static void main(String args[]) throws Exception {
        Scanner myscan = new Scanner(System.in);
        int T = myscan.nextInt();
        for (int i = 0; i < T; i++) {
            int N = myscan.nextInt();
            int[] cows = new int[N];
            int sum = 0;
            for (int j = 0; j < cows.length; j++) {
                cows[j] = myscan.nextInt();
                sum += cows[j];
            }
            ArrayList<Integer> lst = myfun(sum);

            for (Integer z:lst) {
                int mergcount = 0;
                int solution = 0;
                for (int j = 0; j < cows.length; j++) {
                    mergcount += cows[j];
                    if (mergcount > z) {
                        solution = solution + 0;
                        break;
                    } if (mergcount == z) {
                        mergcount = 0;
                        solution++;
                    }
                }
                System.out.println(solution);
            }
        }
        return;
    }

    public static ArrayList<Integer> myfun(int factorable){
        ArrayList<Integer> a = new ArrayList<Integer>();
        for (int i = 2; i < Math.sqrt(factorable); i++) {
            if (0 == factorable % i) {
                a.add(i);
            }            
        }
        return a;
    }
    


}
