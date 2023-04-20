// Rodolfo Rivera
// Project 1
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.ArrayList;

class Project1 {

    public static void checkNeighbors(int x, int y, boolean[][] isVisited, int[][] heights ){
        
    }

    public static ArrayList<String> dfs(int x, int y, int[][] heights) {
        boolean[][] isVisited = new boolean[heights.length][heights[0].length];
        ArrayList<String> ans = new ArrayList<>();
        dfsHelper(heights, isVisited, x, y, ans);
        return ans;
    }

    public static void dfsHelper(int[][] heights, boolean[][] isVisited, int x, int y, ArrayList<String> ans) {
        if (isVisited[x][y]){
            return;
        }
        isVisited[x][y] = true;
        //System.out.println(heights[x][y]);
        int count = 0;
    
        if(y - 1 < 0 || (y - 1 >= 0 && heights[x][y-1] <= heights[x][y])){
            count++; //check north
        }
        if(y+1>=heights.length || (y+1<heights.length && heights[x][y + 1] <= heights[x][y])){
            count++;//check south
        }
        if(x-1<0 || (x-1>=0 && heights[x- 1][y ] <= heights[x][y])){
            count++;//check west
        } 
        if(x+1>= heights[0].length || (x+1< heights[0].length && heights[x+ 1][y ] <= heights[x][y])){
            count++;//check east
        } 
        if(count == 4){
            ans.add(y + " " + x);
        } 
        if(y-1>=0 && heights[x][y-1] >= heights[x][y] && !isVisited[x][y - 1]){
            dfsHelper(heights, isVisited, x, y-1,ans); 
        }
        if (y+1<heights.length && heights[x][y + 1] >= heights[x][y] && !isVisited[x][y + 1]) {
            dfsHelper(heights, isVisited, x, y + 1, ans);
            
        }
        if (x-1>=0 && heights[x- 1][y ] >= heights[x][y] && !isVisited[x - 1][y]) {
            dfsHelper(heights, isVisited, x - 1, y, ans);
            
        }
        if (x+1< heights[0].length && heights[x+ 1][y ] >= heights[x][y] && !isVisited[x + 1][y]) {
            dfsHelper(heights, isVisited, x + 1, y, ans);
            
        }
        
    }

    public static void main(String[] args) {

        Scanner sc = null;
        try {
            // Note the filename is input.txt without any mention of the path
            sc = new Scanner(new File("input.txt"));
        } catch (FileNotFoundException e) {
            System.out.println("Did you forget the input file?");
            System.exit(1);
        }

        int rowSize = sc.nextInt(), colSize = sc.nextInt();
        int cleoX = sc.nextInt(), cleoY = sc.nextInt();
        int[][] heights = new int[colSize][rowSize];

        for (int j = 0; j < rowSize; j++) {
            for (int i = 0; i < colSize; i++) {
                heights[i][j] = sc.nextInt();
            }
        }
        ArrayList<String> ans = new ArrayList<>();
        ans = dfs(cleoX, cleoY, heights);
        System.out.println(ans.size());
       for(String i : ans){
        System.out.println(i);
       }
    }

}