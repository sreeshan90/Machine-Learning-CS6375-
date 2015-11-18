/* Implementation of three startegies in Craps game
 */

/**
 *
 * @author Sreesha Nagaraj
 */
/**
 *
 * @author sreesha.n
 */


public class Craps {

    // uniform random integer in [0, N) 
    public static int uniform(int N) {
        return (int) (Math.random() * N);
    } 

    // return sum of two dice
    public static int sumOfTwoDice() {
        int x = 1 + uniform(6);
        int y = 1 + uniform(6);
        return x + y;
    }


   /***************************************************************************
    * Pass bet:
    *
    * Player rolls two dice. Let x be sum.
    *   - if x is 7 or 11 instant win
    *   - if x is 2, 3, or 12 instant loss
    *   - otherwise player repeatedly rolls two dice until sum is x or 7
    *        if sum is x then win
    *        if sum is 7 then lose
    ***************************************************************************/
    public static boolean winsPassBet() {
        int x = sumOfTwoDice();
        if (x == 7 || x == 11)           return true;
        if (x == 2 || x == 3 || x == 12) return false;

        while (true) {
            int y = sumOfTwoDice();
            if (y == 7) return false;
            if (y == x) return true;
        } 
    }



   /***************************************************************************
    *  Run simulation of pass bet N times  
    *  Output winning percentage.          
    ***************************************************************************/
    
    
    public static ReturnValue evenWager (){
        int N = 10;       // number of pass bets to simulate
        int wager = 100;
        int balance = 1000;
        ReturnValue val = new ReturnValue();
        int i;
        //even wager
       // System.out.println("Even Wager strategy");
        for (i = 1; i <= N; i++){
            boolean result = winsPassBet();
            String outcome = "";
            
            if(balance <= 0 || wager > balance){
                break;
            } 
            if(true == result){
                outcome = "Win";
               // System.out.println(i+" "+balance+" "+wager+" "+outcome);
                balance +=wager;
                
            }
            else{
                outcome = "Loss";
                // System.out.println(i+" "+balance+" "+wager+" "+outcome);
                balance -=wager;
            }
            
           
            wager += 100;
       
           
        }
        
    val.setEndBalance(balance);
    val.setIterations(--i);
    return val;
    }
    
    
    
    public static ReturnValue martingaleSystem (){
        int N = 10;       // number of pass bets to simulate
        int wager = 100;
        int balance = 1000;
        ReturnValue val = new ReturnValue();
        int i;
       // System.out.println("Martingale System");
        for (i = 1; i <= N; i++){
            boolean result = winsPassBet();
            String outcome = "";
            
            if(balance <= 0){
                break;
            } 
            
            if(true == result){
                outcome = "Win";
             //   System.out.println(i+" "+balance+" "+wager+" "+outcome);
                balance +=wager;
                wager += 100;
                
            }
            else{
                outcome = "Loss";
               // System.out.println(i+" "+balance+" "+wager+" "+outcome);
                balance -=wager;
                
                wager = wager*2 ;
                if(balance < wager){
                    wager = balance;
                }
            }
           
        }
    val.setEndBalance(balance);
    val.setIterations(--i);
    return val;
    }
    
    
    
    public static ReturnValue reverseMartingaleSystem (){
        int N = 10;       // number of pass bets to simulate
        int wager = 100;
        int balance = 1000;
        ReturnValue val = new ReturnValue();
        int i;
       // System.out.println("Reverse Martingale System");
        for ( i = 1; i <= N; i++){
            boolean result = winsPassBet();
            String outcome = "";
            
            if(balance <= 0){
                break;
            } 
            
            if(true == result){
                outcome = "Win";
             //   System.out.println(i+" "+balance+" "+wager+" "+outcome);
                balance +=wager;
                wager = wager*2 ; // double 
               
                
            }
            else{
                outcome = "Loss";
              //  System.out.println(i+" "+balance+" "+wager+" "+outcome);
                balance -=wager;
                 wager = 100; //keep constant
                
                if(balance < wager){
                    wager = balance;
                }
            }
           
        }
    val.setEndBalance(balance);
    val.setIterations(--i);
    return val;
    }
    public static void main(String[] args) { 

        for(int i=1;i<=5;i++){
            
            System.out.println("Round "+i);
            ReturnValue ret1 = evenWager();
            ReturnValue ret2 = martingaleSystem();
            ReturnValue ret3 = reverseMartingaleSystem();
            
            System.out.println("1"+" "+ret1.getIterations()+" $"+ret1.getEndBalance());
            System.out.println("2"+" "+ret2.getIterations()+" $"+ret2.getEndBalance());
            System.out.println("3"+" "+ret3.getIterations()+" $"+ret3.getEndBalance());
            
        }
         
    }

}
