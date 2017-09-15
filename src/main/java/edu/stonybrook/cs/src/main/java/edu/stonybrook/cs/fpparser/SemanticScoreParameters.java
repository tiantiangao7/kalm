package main.java.edu.stonybrook.cs.fpparser;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import main.java.edu.stonybrook.cs.thread.BabelNetShareResource;

public class SemanticScoreParameters {
	private static final String input = "resources/semantic_score_meta/edge_param.txt";
	private static HashMap<String, Double> weightMap = new HashMap<String,Double>();
	private static HashMap<String, Double> penaltyMap = new HashMap<String,Double>();
	
	public static void initWeightBias()
	{
		weightMap.put("hypernym", 0.1);
		weightMap.put("hyponym", 0.05);
		weightMap.put("Derivationally related form", 0.1);
		weightMap.put("Gloss related form (disambiguated)", 0.07);
		weightMap.put("Gloss related form (monosemous)", 0.07);
	}
	
	public static void setFuzzyIsAWeightBias(String frameName, String frameElementName, String edgeType, Double val)
	{
		String[] sidList = FrameDescriptionPredicate.GetFramePropertySID(frameName, frameElementName);
		assert sidList != null;
		String ParameterPrefix = Arrays.toString(sidList);
		System.out.println(ParameterPrefix);
		weightMap.put(ParameterPrefix + "::" + edgeType, val);
	}
	
	public static void initPenaltyVal()
	{
		penaltyMap.put("hypernym", 0.5);
		penaltyMap.put("Derivationally related form", 0.7);
		penaltyMap.put("Gloss related form (disambiguated)", 1.0);
		penaltyMap.put("Gloss related form (monosemous)", 1.0);
		penaltyMap.put("hyponym", 2.0);
		penaltyMap.put("holonym", 2.0);
		penaltyMap.put("meronym", 2.0);
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
	
	public static Double getInverseWeightBias(String weightKey)
	{	
		String[] array = weightKey.split("::");
		assert array.length == 2;
		String newWeightKey = array[0] + "::" + BabelNetShareResource.getInverseEdgeName(array[1]);
		return getWeightBias(newWeightKey);
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
	
	public static Double getInversePenaltyVal(String penaltyKey)
	{
		String[] array = penaltyKey.split("::");
		assert array.length == 2;
		String newPenaltyKey = array[0] + "::" + BabelNetShareResource.getInverseEdgeName(array[1]);	
		return getPenaltyVal(newPenaltyKey);
	}

	private static boolean ParseEdgeWeightBiasPredicate(String input)
	{
	     Matcher m = Pattern.compile("^weight_bias\\(\\'(.+)\\',(.+)\\).$").matcher(input);
	     while(m.find()) {	    	 
	    	 weightMap.put(m.group(1), Double.parseDouble(m.group(2)));
	    	 return true;
	     }
	     return false;
	}
	
	private static boolean ParseEdgePenaltyPredicate(String input)
	{
	     Matcher m = Pattern.compile("^penalty\\(\\'(.+)\\',(.+)\\).$").matcher(input);
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
				else
				{
					ParseEdgePenaltyPredicate(sentence);
				}
			}		
		}
		catch (IOException x) 
		{
		      System.err.println(x);
		}
	}
	
	public static void main(String[] args) {
		FrameDescriptionPredicate.Parse();
		initWeightBias();
		initPenaltyVal();
		setFuzzyIsAWeightBias("Cooking","Place","hypernym", 6.0);
		setFuzzyIsAPenaltyVal("Create_Organization","Organization","hypernym", 0.0009);
		System.out.println(getWeightBias("[bn:00059480n]::hypernym"));
		System.out.println(getPenaltyVal("[bn:00000704n]::hypernym"));
	}
}
