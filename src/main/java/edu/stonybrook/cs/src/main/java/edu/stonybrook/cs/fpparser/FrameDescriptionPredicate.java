package main.java.edu.stonybrook.cs.fpparser;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import main.java.edu.stonybrook.cs.frame.Frame;
import main.java.edu.stonybrook.cs.frameextraction.FrameExtractor;
import main.java.edu.stonybrook.cs.util.PrologConnector;

public class FrameDescriptionPredicate {
	private static final String input = "resources/frame_property/frame_property.txt";
	private static HashMap<String, ArrayList<PropertyPredicate>> frameDescriptionMap = 
			new HashMap<String, ArrayList<PropertyPredicate>>();
	
	FrameDescriptionPredicate()
	{
		
	}
	
	public static void Parse()
	{
		try (BufferedReader br = new BufferedReader(new FileReader(input))) 
		{			
			String sentence;			
			while((sentence = br.readLine())!=null)
			{
				System.out.println(sentence);
				ParseFrameDescription(sentence);       
			}
			
		}
		catch (IOException x) 
		{
		      System.err.println(x);
		}
	}
	
	private static void ParseFrameDescription(String input)
	{
		 ArrayList<PropertyPredicate> list = new ArrayList<PropertyPredicate>();
		 String frameName = null;
	     Matcher m = Pattern.compile("^fp\\(\\'([a-zA-Z_]+)\\',\\[(.*)\\]\\).$").matcher(input);
	     while(m.find()) {
	       frameName = m.group(1);
	       String propertyList = m.group(2);
	       Matcher m2 = Pattern.compile("property\\(\\'(.*?)\\',(.*?)\\)").matcher(propertyList);
	       while(m2.find()) {
	    	   PropertyPredicate propertyPredicate = new PropertyPredicate(m2.group(1), m2.group(2));
	    	   list.add(propertyPredicate);
	       }	       	       
	     }
	     frameDescriptionMap.put(frameName, list);
	}
	
	public static ArrayList<PropertyPredicate> GetFramePropertyList(String frameName)
	{
		return frameDescriptionMap.get(frameName);
	}
	
	public static String[] GetFramePropertySID(String frameName, String frameElementName)
	{
		ArrayList<PropertyPredicate> propertyList = frameDescriptionMap.get(frameName);
		for(PropertyPredicate propertyPredicate : propertyList)
		{
			if(propertyPredicate.GetPropertyName().equals(frameElementName))
			{
				return propertyPredicate.GetBabelNetSID();
			}
		}
		return null;
	}
}
