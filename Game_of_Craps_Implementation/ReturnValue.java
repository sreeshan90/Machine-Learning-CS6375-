/*Class used as the return value from functions that implement different strategies 
 */

/**
 *
 * @author Sreesha Nagaraj
 */
public class ReturnValue{
    private int iterations;
    private int endBalance;

    public int getIterations() {
        return iterations;
    }

    public void setIterations(int iterations) {
        this.iterations = iterations;
    }

    public int getEndBalance() {
        return endBalance;
    }

    public void setEndBalance(int endBalance) {
        this.endBalance = endBalance;
    }
}