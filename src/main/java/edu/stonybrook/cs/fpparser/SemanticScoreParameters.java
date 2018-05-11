package main.java.edu.stonybrook.cs.fpparser;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SemanticScoreParameters {
	private static final String input = "resources/semantic_score_meta/edge_param.txt";
	private static HashMap<String, Double> weightMap = new HashMap<String,Double>();
	private static HashMap<String, Double> penaltyMap = new HashMap<String,Double>();
	
	public static void setFuzzyIsAWeightBias(String frameName, String frameElementName, String edgeType, Double val)
	{
		String[] sidList = FrameDescriptionPredicate.GetFramePropertySID(frameName, frameElementName);
		assert sidList != null;
		String ParameterPrefix = Arrays.toString(sidList);
		System.out.println(ParameterPrefix);
		weightMap.put(ParameterPrefix + "::" + edgeType, val);
	}
	
	public static void setFuzzyIsAPenaltyVal(String frameName, String frameElementName, String edgeType, Double val)
	{
		String[] sidList = FrameDescriptionPredicate.GetFramePropertySID(frameName, frameElementName);
		assert sidList != null;
		String ParameterPrefix = Arrays.toString(sidList);
		System.out.println(ParameterPrefix);
		penaltyMap.put(ParameterPrefix + "::" + edgeType, val);
	}
	
	public static Double getWeightBias(String weightKey)
	{
		if(weightMap.containsKey(weightKey))
		{
			return weightMap.get(weightKey);
		}
		else
		{
			String[] array = weightKey.split("::");
			assert array.length == 2;
			if(weightMap.containsKey(array[1]))
			{
				return weightMap.get(array[1]);
			}
			else 
			{
				return 0.0;
			}
		}
	}
	
	public static Double getPenaltyVal(String penaltyKey)
	{
		if(penaltyMap.containsKey(penaltyKey))
		{
			return penaltyMap.get(penaltyKey);
		}
		else
		{
			String[] array = penaltyKey.split("::");
			assert array.length == 2;
			if(penaltyMap.containsKey(array[1]))
			{
				return penaltyMap.get(array[1]);
			}
			else
			{
				return 5.0;
			}
		}
	}

	private static boolean ParseEdgeWeightBiasPredicate(String input)
	{
	     Matcher m = Pattern.compile("^weight_bias\\(\\'(.+)\\',(.+)\\)\\.$").matcher(input);
	     while(m.find()) {	    	 
	    	 weightMap.put(m.group(1), Double.parseDouble(m.group(2)));
	    	 return true;
	     }
	     return false;
	}
	
	private static boolean ParseEdgePenaltyPredicate(String input)
	{
	     Matcher m = Pattern.compile("^penalty\\(\\'(.+)\\',(.+)\\)\\.$").matcher(input);
	     while(m.find()) {
	    	 penaltyMap.put(m.group(1), Double.parseDouble(m.group(2)));
	    	 return true;
	     }
	     return false;
	}
	
	public static void initialize()
	{
		try (BufferedReader br = new BufferedReader(new FileReader(input))) 
		{			
			String sentence;			
			while((sentence = br.readLine())!=null)
			{
				if(ParseEdgeWeightBiasPredicate(sentence))
				{
					continue;
				}
				else if(ParseEdgePenaltyPredicate(sentence))
				{
					continue;
				}
				else
				{
					throw new AssertionError("invalid predicate.");
				}
			}		
		}
		catch (IOException x) 
		{
		      System.err.println(x);
		}
	}
}
