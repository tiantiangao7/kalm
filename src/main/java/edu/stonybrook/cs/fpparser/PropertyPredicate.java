package main.java.edu.stonybrook.cs.fpparser;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PropertyPredicate {
	private String propertyName = null;
	private String[] synsetList = null;
	private String[] dataTypeList = null;
	private String[] prohibitedEdgeList = null;
	
	PropertyPredicate(String PropertyName, String PropertyDescriptionValues)
	{
		this.propertyName = PropertyName;
		Matcher m = Pattern.compile("\\[(.*?)\\]").matcher(PropertyDescriptionValues);
		int counter = 0;
	    while(m.find()) {
	    	counter++;
	    	if(counter == 1)
	    	{
	    		SetBabelNetSID(m.group(1));
	    	}
	    	else if (counter == 2)
	    	{
	    		SetDataTypeConstraint(m.group(1));
	    	}
	    	else if(counter == 3)
	    	{
	    		SetProhibitedEdgeList(m.group(1));
	    	}
	    }
	}
	
	public String GetPropertyName()
	{
		return propertyName;
	}
	
	private void SetBabelNetSID(String input)
	{
		synsetList = GetElementFromList(input);
	}
	
	private void SetDataTypeConstraint(String input)
	{
		dataTypeList = GetElementFromList(input);
	}
	
	private void SetProhibitedEdgeList(String input)
	{
		prohibitedEdgeList = GetElementFromList(input);
	}
	
	private String[] GetElementFromList(String input)
	{
		String[] tmpList = null;
		if(input.length() == 0)
		{
			return tmpList;
		}
		tmpList = input.split(",");
		for(int i = 0; i < tmpList.length; i++)
		{
			assert(tmpList[i].charAt(0) == '\'');
			assert(tmpList[i].charAt(tmpList[i].length()-1) == '\'');
			tmpList[i] = tmpList[i].substring(1, tmpList[i].length() - 1);
		}
		return tmpList;
	}
		
	public String[] GetBabelNetSID()
	{
		return synsetList;
	}
	
	public String[] GetDataTypeConstraint()
	{
		return dataTypeList;
	}
	
	public String[] GetProhibitedEdgeList()
	{
		return prohibitedEdgeList;
	}
}
