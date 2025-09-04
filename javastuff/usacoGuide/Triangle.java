import java.util.*;
import java.io.*;
class Triangle {
    public static void main(String args[]) throws Exception {
        Scanner myscan = new Scanner(new File("Triangle.in"));
		PrintWriter writefile = new PrintWriter("Triangle.out");
        int n = myscan.nextInt();
        int[][] trianglearray = new int[n][2];

        for (int i = 0; i < n; i++) {
            trianglearray[i][0] = myscan.nextInt();
            trianglearray[i][1] = myscan.nextInt();
        }
        int highesthight;
        for (int i = 0; i < trianglearray.length; i++) {
            for (int j = i+1; j < trianglearray.length; j++) {
                for (int k = 1+j; k < trianglearray.length; k++) {
                    if (trianglearray[i][0] == trianglearray[j][0]) {
                        
                    }
                }
            }
        }
		writefile.print(maxbuckets);
		writefile.close();
        return;
    }

    public static int helper(int ogpoints [][]) throws Exception {
        int[] p1 = ogpoints[0]; 
        int[] p2 = ogpoints[1]; 
        int[] p3 = ogpoints[2]; 
        if ((p1[0] == p2[0] && p1[1] == p3[1]) || (p1[0] == p3[0] && p1[1] == p2[1]) || (p2[0] == p1[0] && p1[1] == p3[1])) {return (p1[0] * p2[1] + p2[0] * p3[1] + p3[0] * p1[1] - p1[1] * p2[0] + p2[1] * p3[0] + p3[1] * p1[0]);}
        if (p2[0] == p2[0] && p1[1] == p3[1]) {
            return (p1[0] * p2[1] + p2[0] * p3[1] + p3[0] * p1[1] - p1[1] * p2[0] + p2[1] * p3[0] + p3[1] * p1[0]);}
        
        return null;
    }
}