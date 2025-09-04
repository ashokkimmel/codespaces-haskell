import java.io.*;
import java.util.*;

public class ShapeFinder {
public static void main(String[] args) {
Scanner myscan = new Scanner(System.in);
int n = myscan.nextInt();
int [][] arrays = new int[n][2];
for (int i = 0; i < arrays.length; i++) {
    arrays[i][0] = myscan.nextInt();
    arrays[i][1] = myscan.nextInt();
}
int ans = 0;
for (int i = 0; i < arrays.length; i++) {
a = arrays[i];
for (int j = i+1; j < arrays.length; j++) {
int[] b = arrays[j];
int ab = dist(a,b);        
for (int k = j+1; k < arrays.length; k++) {
int[] c = arrays[k];
int ac = dist(a,c);        
int bc = dist(b,c);        
for (int l = k+1; l < arrays.length; l++) {
int[] d = arrays[l];
ad = dist(a,d);        
bd = dist(b,d);        
cd = dist(c,d);        
boolean acsquare,absquare,adsquare;
acsquare = ab == bc && ab == cd && ab == ad && ac == bd && ac == 2 * ab;
absquare = ac == bc && ac == ad && ac == bd && ab == cd && ab == 2 * ac;
adsquare = ab == ac && ab == cd && ab == bd && bc == ad && bc == 2 * ab;
if (acsquare || absquare || adsquare) {ans++;}
}}}}
System.out.print(ans);} 
static int dist(int[] num1,int [] num2) {
    int dist = (num1[0] - num2[0]) * (num1[0] - num2[0]) + (num1[1] - num2[1]) * (num1[1] - num2[1]);
    return dist;}}