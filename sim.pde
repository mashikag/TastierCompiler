void loopThrough(int[] arr){
  int totLen = 1;
  for(int i = 0; i < arr.length; i++)
    totLen *= arr[i];
  
  System.out.println("Total Length: " + totLen);
  
  String name = "arr";
  
  int[] tArr = new int[arr.length];
  System.arraycopy( arr, 0, tArr, 0, arr.length );
  for(int i = 0; tArr.length > i; i++)
    tArr[i] = 0;
    
  int count = 0;
  boolean finished = false;
  while(!finished){
    count++;
    System.out.print(count + " => " + name);
    for(int i = 0; i < tArr.length; i++)
      System.out.print("[" + tArr[i] + "]");
    System.out.print("; ");
    
    tArr[tArr.length-1]++;
    tArr[tArr.length-1] = tArr[tArr.length-1] % arr[tArr.length-1];
    for(int c = (tArr.length-1); tArr[c]==0;){
      tArr[c] = 0;
      c--;
      if(c < 0){
        finished = true;
        break;
      }
      tArr[c]++;
      tArr[c] = tArr[c]%arr[c];
    }
  }
}

void setup(){
  int[] test = {3,4,5};
  loopThrough(test);
}
