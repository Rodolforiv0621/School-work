import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

class project1{
    
    public static void main(String []args){
        Queue<Integer> q = new LinkedList<>();
        Scanner sc = null;
        try {
        //Note the filename is input.txt without any mention of the path
        sc = new Scanner(new File("input.txt"));
        }
        catch(FileNotFoundException e) {
        System.out.println("Did you forget the input file?");
        System.exit(1);
        }

        int posRow=0, posCol = 0, rowSize = sc.nextInt(), colSize = sc.nextInt();
        int cleoX = sc.nextInt(), cleoY = sc.nextInt();
        int[][] theData = new int[rowSize][colSize];

        for (int j = 0;j<rowSize;j++) {
            for(int i = 0;i<colSize;i++){
                theData[i][j] = sc.nextInt();
            }
        }
        System.out.println(theData[cleoX][cleoY]);
        q.add(theData[2][3]);
        System.out.println(q.peek());
    }
    
}