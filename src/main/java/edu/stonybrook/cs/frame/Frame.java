package main.java.edu.stonybrook.cs.frame;

import java.util.ArrayList;

public class Frame {
	private String FrameName;
	private ArrayList<FrameElement> feList = new ArrayList<FrameElement>();
	private double score = 0.0;
	private String scoreStr = null;
	
	public Frame(String FrameName)
	{
		this.FrameName = FrameName;
	}
	
	public String getFrameName()
	{
		return FrameName;
	}
	
	public void addFrameElement(FrameElement fe)
	{
		feList.add(fe);
	}
	
	public FrameElement getFrameElementbyName(String FEName)
	{
		FrameElement fe = null;
		for(int i = 0; i < feList.size(); i++)
		{
			if(FEName.equals(feList.get(i).getFEName()))
			{
				fe = feList.get(i);
				break;
			}
		}
		return fe;
	}
	
	public ArrayList<FrameElement> getAllFrameElements()
	{
		return feList;
	}
	
	public void resetScore()
	{
		int len = 0;
		score = 1.0;
		for(FrameElement fe : feList)
		{
			if(fe.getFEVal() != null)
			{
				len++;
				score = score * fe.getMainSynsetAffinityScore();
			}
		}
		score = Math.pow(score, 1.0/len);	
		scoreStr = String.format("%.3f", score);
	}
	
	public String getScore()
	{
		return scoreStr;
	}
	
	public boolean IsZeroScore()
	{
		if(score > 0.0)
		{
			return false;
		}
		else
		{
			return true;
		}
	}
	
	public String print()
	{
		String s = "[" + FrameName + "]" + score +  "\n";
		for(FrameElement fe : feList)
		{
			s += fe.print();
		}
		return s;
	}
}
