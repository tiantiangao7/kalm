package main.java.edu.stonybrook.cs.frame;

import java.util.ArrayList;
import java.util.Set;

import main.java.edu.stonybrook.cs.query.QueryProcessing;

public class Frame {
	private String FrameName;
	private ArrayList<FrameElement> feList = new ArrayList<FrameElement>();
	private double score = 0.0;
	private String scoreStr = null;
	private double coefficient = 1.0;
	
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
	
	public void resetFrameScore()
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
		score = Math.pow(score, 1.0/len)*coefficient;	
		scoreStr = String.format("%.3f", score);
	}
	
	public double getFrameScore()
	{
		return score;
	}
	
	public String getFrameScoreStr()
	{
		return scoreStr;
	}
	
	public int getNonEmptyFENum()
	{
		int num = 0;
		for(FrameElement fe : feList)
		{
			if(fe.getFEVal() != null)
			{
				num++;
			}
		}
		return num;
	}
	
	public void setCoefficientVal(double val)
	{
		coefficient = val;
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

	public String getULR()
	{	
		if(!QueryProcessing.IsVarWordIndexSetEmpty())
		{
			return getULRForQuery();
		}
		else
		{
			return getULRForFacts();
		}
	}
	
	private String getULRForFacts()
	{
		int count = 2;
		String prefix = "id_";
		String ulr = "";
		
		ulr += "frame(" + prefix + 1 + ", " + FrameName + ").\n";
		for(FrameElement fe : feList)
		{
			if(fe.getFEVal() == null)
			{
				continue;
			}				
			ulr += "role(" + prefix + 1 + ", " + fe.getFEName() + ", " + prefix + count + ").\n";								
			ulr += "entity(" + prefix + count + ", " + fe.getFEValBabelSynsetID() + ").\n";
			ulr += "text(" + prefix + count + ", " + fe.getFEVal() + ").\n";		
			count++;
		}
		System.out.println(ulr);
		return ulr;
	}
	
	private String getULRForQuery()
	{
		int count = 2;
		String prefix = "X";
		String ulr = "";

		ulr += "frame(" + prefix + 1 + ", " + FrameName + ")";
		for(FrameElement fe : feList)
		{
			if(fe.getFEVal() == null)
			{
				continue;
			}
			if(!QueryProcessing.IsInVarWordIndexSet(fe.getFEValWordIndex()))
			{
				ulr += ",\n";
				ulr += "role(" + prefix + 1 + ", " + fe.getFEName() + ", " + prefix + count + "),\n";
				ulr += "entity(" + prefix + count + ", " + fe.getFEValBabelSynsetID() + ")";
			}
			else
			{
				ulr += ",\n";
				ulr += "role(" + prefix + 1 + ", " + fe.getFEName() + ", " + prefix + count + "),\n";
				ulr += "entity(" + prefix + count + ", " + "S" + count + "),\n";
				if(fe.getFEVal().equals("what"))
				{
					ulr += "type(" + "S" + count + ", " + fe.getFENameBabelSynsetID() + ")";
				}
				else
				{
					ulr += "type(" + "S" + count + ", " + fe.getFEValBabelSynsetID() + ")";
				}
			}
			if(QueryProcessing.IsInVarWordIndexSet("q" + fe.getFEValWordIndex()))
			{
				ulr += ",\n";
				ulr += "modifier(" + prefix + count + ", quantity, " + "Z" + count + ")";
			}
			count++;
		}
		ulr += ".\n";
		System.out.println(ulr);
		return ulr;
	}
	
	public String getTopResult()
	{
		String s = "'" + FrameName + "',[";
		boolean flag = false;
		for(FrameElement fe : feList)
		{
			String temp = fe.getTopResult();
			if(temp != null)
			{
				if(flag == false)
				{
					flag = true;
					s += fe.getTopResult();
				}
				else
				{
					s += "," + fe.getTopResult();
				}
			}
		}
		return s + "]," + scoreStr;
	}
	
	public String getTopResultWithoutScore()
	{
		String s = "'" + FrameName + "',[";
		boolean flag = false;
		for(FrameElement fe : feList)
		{
			String temp = fe.getTopResult();
			if(temp != null)
			{
				if(flag == false)
				{
					flag = true;
					s += fe.getTopResult();
				}
				else
				{
					s += "," + fe.getTopResult();
				}
			}
		}
		return s + "],";
	}
}
