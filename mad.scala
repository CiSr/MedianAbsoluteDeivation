import scala.math._

class MAD(input_list:List[Int]){



//Check Normality

	def CheckNormal():Boolean={
	//Compute the mean
		val x=input_list
		val mean=x.foldLeft(0)(_+_)/x.length
	//Find the Standard Deviation
		val std_list=x.map(x=>(x-mean) * (x-mean))//.map(x=>x*2)
		val std_deviation=std_list.foldLeft(0)(_+_)/std_list.length
	//Check whether lies in between 3sigma
		val lrange_normal=mean-3*std_deviation
		val urange_noraml=mean+3*std_deviation
	//Returns an empty list if the values are no values
		val temp=x.filterNot(x=> x >lrange_normal && x < urange_noraml)
	//If the list returned is empty then the values lie btw mean+-3sigma
		val normal_flag=if(temp.isEmpty)true else false
		normal_flag
}




//Find the Median
	def return_median(x:List[Int],low:Int,high:Int):Int=
	{
		//Check Whether List is Odd or Even 
		if(high%2!=0){
			//println("List with Odd Elements:")
			val mid=math.floor((low+high)/2).toInt
			val median_val=(x(mid)+x(mid+1))/2
			median_val
		}
		else
		{		
			//println("List with Odd Elements:")
			val median_val=x((math.floor((low+high)/2)).toInt)
			median_val
		}	
	}



	def outliers_detect(){
//Subtract the median with all values
		val median=return_median(input_list.sorted,0,input_list.length-1)
		val modified_list=input_list.map(x=>math.abs(x-median))
		val revised_median=return_median(modified_list.sorted,0,modified_list.length-1)

	//Setup b value
		val b=1.4826
		val threshold_value=revised_median*b
		val upper_limit=median+2*threshold_value
		val lower_limit=median-2*threshold_value

	//Filter the list with the upper and lower limits
		val outlier_list=input_list.filter(x=>x<lower_limit || x>upper_limit)
		println(outlier_list)
}



}


object mad{
	def main(args: Array[String]){
		println("Running.....")
		val input_list=List(2, 6, 6, 12, 17, 25 ,32)
		println("The inputList is:"+" "+input_list)
		val mad_obj=new MAD(input_list)
		val flag=mad_obj.CheckNormal()
		println("The Dataset is Normal"+"::" +" "+flag)
		println()
		println("The Outliers are returned as: ")
		mad_obj.outliers_detect()
		
	
}
}