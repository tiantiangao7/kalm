package main.java.edu.stonybrook.cs.frame;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.List;
import it.uniroma1.lcl.babelnet.BabelSynset;
import main.java.edu.stonybrook.cs.thread.BabelNetCache;
import main.java.edu.stonybrook.cs.thread.BabelNetShareResource;
import main.java.edu.stonybrook.cs.thread.InverseSemanticScoreThread;
import main.java.edu.stonybrook.cs.thread.SemanticScoreThread;
import main.java.edu.stonybrook.cs.util.BabelNetConnector;

public class FrameElement {
	private String FEName;
	private String[] FENameBabelSynsetIDList;
	private String FENameBabelSynsetGloss;
	private String FEVal;
	private String FEValPos;
	private String FEValBabelSynsetID;
	private String FEValBabelSynsetGloss;
	private Double FEValBabelSynsetScore;
	private String FrameElementKey;
	private List<String> FEValBabelSynsetIDList;
	private List<String> FEValBabelSynsetGlossList;
	private List<Tuple> tupleList;
	private String DataType; 
	private Hashtable<String,String> synsetToGloss = new Hashtable<String,String>();
	private Hashtable<String,Double> synsetToScore = new Hashtable<String,Double>();
	private Hashtable<String,String> synsetToPath = new Hashtable<String,String>();
	private Hashtable<String, Integer> FENameBabelSynsetIDMap = new Hashtable<String,Integer>();
	private ArrayList<String> prohibitedEdgeList;
	
	static class Tuple{
		public String synsetID = null;
		public String gloss = null;
		public double score = 0.0;
	}
	
	public FrameElement(String FEName, String[] FENameBabelSynsetIDList)
	{
		this.FEName = FEName;
		this.FENameBabelSynsetIDList = FENameBabelSynsetIDList; 
		this.FEVal = null;
		this.FEValPos = null;
		this.FEValBabelSynsetID = null;
		this.FEValBabelSynsetGloss = null;
		this.FEValBabelSynsetScore = 0.0;
		this.FEValBabelSynsetIDList = new ArrayList<String>();
		this.FEValBabelSynsetGlossList = new ArrayList<String>();	
		BabelSynset bs = BabelNetConnector.getBabelNetSynset(FENameBabelSynsetIDList[0]);
		this.FENameBabelSynsetGloss = BabelNetConnector.getMainGloss(bs);
		this.DataType = null;
		this.FrameElementKey = null;
		this.tupleList = new ArrayList<Tuple>();
		this.prohibitedEdgeList = null;
		
		for(String sid : FENameBabelSynsetIDList)
		{
			FENameBabelSynsetIDMap.put(sid, 1);
		}
	}
	
	public void setFEValBabelSynsetID()
	{
		if(FEValBabelSynsetIDList.size() > 0) 
		{
			this.FEValBabelSynsetID = FEValBabelSynsetIDList.get(0);
			this.FEValBabelSynsetGloss = FEValBabelSynsetGlossList.get(0);
			this.FEValBabelSynsetScore = synsetToScore.get(FEValBabelSynsetID);
		}
	}
	
	public void changeFEValBabelSynsetID(String FEValBabelSynsetID)
	{
		this.FEValBabelSynsetID = FEValBabelSynsetID;
		this.FEValBabelSynsetGloss = synsetToGloss.get(FEValBabelSynsetID);
		this.FEValBabelSynsetScore = synsetToScore.get(FEValBabelSynsetID);
	}
	
	public List<Thread> createSematicScoreComputationThreads()
	{
		// To-do: 1) check if there is no requirement to compute the semantic score for a FE Val
		//        2) return if one of the synsets related to FE equals the FE synset
		if(checkInteger(FEVal) && DataType == null)
		{
			FEValBabelSynsetIDList.add(FEVal);
			FEValBabelSynsetGlossList.add("an integer");
			synsetToScore.put(FEVal, 0.0);
			synsetToGloss.put(FEVal, "an integer");
			synsetToPath.put(FEVal, "");
			return null;
		}
		
		if(checkInteger(FEVal) && DataType != null && DataType.equals("Integer"))
		{
			FEValBabelSynsetIDList.add(FEVal);
			FEValBabelSynsetGlossList.add("an integer");
			synsetToScore.put(FEVal, 1.0);
			synsetToGloss.put(FEVal, "an integer");
			synsetToPath.put(FEVal, "");
			return null;
		}
		
		List<BabelSynset> FEValBabelSynsetList = GetBabelNetQueryResult();
		boolean hasGivenName = false;
		for(BabelSynset bs : FEValBabelSynsetList)
		{
			if(!hasGivenName && BabelNetConnector.isGivenName(bs))
			{
				hasGivenName = true;
			}
			
			String synsetID = bs.getId().getID();
			String gloss = BabelNetConnector.getMainGloss(bs);
			if(gloss != null && gloss.length() != 0)
			{
				if(bs.isKeyConcept())
				{
					if(FEValPos.equals("ne"))
					{
						continue;
					}
					gloss = "KC - " + gloss;
				}
				else if(bs.getSynsetType().name().equals("NAMED_ENTITY"))
				{
					if(!FEValPos.equals("ne")&&!FEValPos.equals("a"))
					{
						continue;
					}
					gloss = "NE - " + gloss;
				}
				else
				{
					if(FEValPos.equals("ne"))
					{
						continue;
					}
					gloss  = "C - " + gloss;
				}
			}
			synsetToGloss.put(synsetID, gloss);
			Tuple tuple = new Tuple();
			tuple.synsetID = synsetID;
			tuple.gloss = gloss;
			tupleList.add(tuple);
		}
		String personSynsetID = "bn:00046516n";
		if(hasGivenName && !synsetToGloss.contains(personSynsetID))
		{
			Tuple tuple = new Tuple();
			tuple.synsetID = personSynsetID;
			tuple.gloss = "A human being";
			synsetToGloss.put(tuple.synsetID, tuple.gloss);
			tupleList.add(tuple);			
		}
		
		if(IsFEValMatchFEName(tupleList))
		{
			tupleList.clear();
			return null;
		}
		
		BabelNetShareResource.clear();
		BabelNetCache.clear();
		List<Thread> threadPool = new ArrayList<Thread>();
		String[] tempFENameList = new String[tupleList.size()];
		for(int i = 0; i < tempFENameList.length; i++)
		{
			tempFENameList[i] = tupleList.get(i).synsetID;
		}
		for(String sid : FENameBabelSynsetIDList)
		{
			Thread invThread = 
					new InverseSemanticScoreThread(Arrays.toString(FENameBabelSynsetIDList), FrameElementKey,
							sid, tempFENameList, prohibitedEdgeList);
			threadPool.add(invThread);
		}
		for(int i = 0; i < tupleList.size(); i++)
		{
			String synsetID = tupleList.get(i).synsetID;
			Thread t = new SemanticScoreThread(FrameElementKey,
					i, synsetID, FENameBabelSynsetIDList, prohibitedEdgeList);
			threadPool.add(t);
		}
		return threadPool;
	}
	
	private List<BabelSynset> GetBabelNetQueryResult()
	{
		if(FEValPos.equals("a"))
		{
			List<BabelSynset> FEValBabelSynsetList = 
				BabelNetConnector.getBabelNetSynsets(FEVal, FEValPos);
			List<BabelSynset> FEValBabelSynsetList2 = 
					BabelNetConnector.getBabelNetSynsets(FEVal, "n");			
			FEValBabelSynsetList.addAll(FEValBabelSynsetList2);
			return FEValBabelSynsetList;
		}
		else if(FEValPos.equals("ne"))
		{
			List<BabelSynset> FEValBabelSynsetList = 
					BabelNetConnector.getBabelNetSynsets(FEVal, "n");			
			return FEValBabelSynsetList;
		}
		else
		{
			List<BabelSynset> FEValBabelSynsetList = 
					BabelNetConnector.getBabelNetSynsets(FEVal, FEValPos);
			return FEValBabelSynsetList;
		}
	}
	
	public void setFEValBabelSynsetIDList()
	{	
		if(tupleList.size() == 0) 
		{
			return;
		}
		
		for(int i = 0; i < tupleList.size(); i++)
		{
			String synsetID = tupleList.get(i).synsetID;
			double score = BabelNetShareResource.getScore(FrameElementKey, i);
			String path = BabelNetShareResource.getPath(FrameElementKey, i);
			synsetToScore.put(synsetID, score);
			synsetToPath.put(synsetID, path);
			tupleList.get(i).score = score;
		}		
		
		Collections.sort(tupleList, new Comparator<Tuple>(){
	        public int compare(Tuple i1, Tuple i2){
	            if(i1.score == i2.score)
	            {
	                return 0;
	            }
	            else if(i1.score < i2.score)
	            {	
	                return 1;
	            }
	            else
	            {
	            	return -1;
	            }
	        }
	    });
		
		for(Tuple tuple : tupleList)
		{
			FEValBabelSynsetIDList.add(tuple.synsetID);
			FEValBabelSynsetGlossList.add(tuple.gloss);
			System.out.println(tuple.synsetID+"-"+FENameBabelSynsetIDList[0]+" "+tuple.score);
		}
	}
	
	private boolean IsFEValMatchFEName(List<Tuple> tupleList)
	{
		boolean isMatched = false;
		for(Tuple tuple : tupleList)
		{
			if(FENameBabelSynsetIDMap.containsKey(tuple.synsetID))
			{
				isMatched = true;
				FEValBabelSynsetIDList.add(tuple.synsetID);
				FEValBabelSynsetGlossList.add(tuple.gloss);
				synsetToScore.put(tuple.synsetID, 1.0);
				synsetToPath.put(tuple.synsetID, "");
				break;
			}
		}		
		if(isMatched)
		{
			for(Tuple tuple : tupleList)
			{
				if(!FENameBabelSynsetIDMap.containsKey(tuple.synsetID))
				{
					FEValBabelSynsetIDList.add(tuple.synsetID);
					FEValBabelSynsetGlossList.add(tuple.gloss);
					synsetToScore.put(tuple.synsetID, 0.0);
					synsetToPath.put(tuple.synsetID, "");
				}
			}
			return true;
		}
		else
		{
			return false;
		}
	}
	
	public void setFEVal(String FEVal, String FEValPos)
	{
		this.FEVal = FEVal;
		this.FEValPos = FEValPos;
		String temp = "";
		for(String sid : FENameBabelSynsetIDList)
		{
			temp += sid;
		}
		this.FrameElementKey = FEVal + ":" + temp;
	}
	
	public double getMainSynsetAffinityScore()
	{
		return this.FEValBabelSynsetScore;
	}
	
	public String getFEName()
	{
		return FEName;
	}
	
	public String getFEVal()
	{
		return FEVal;
	}
	
	public String getFENameBabelSynsetID()
	{
		return FENameBabelSynsetIDList[0];
	}
	
	public String getFEValBabelSynsetID()
	{
		return FEValBabelSynsetID;
	}
	
	public String getFEValBabelSynsetGloss()
	{
		return FEValBabelSynsetGloss;
	}
	
	public String getFEValBabelSynsetID(int index)
	{
		return FEValBabelSynsetIDList.get(index);
	}
	
	public String getFEValBabelSynsetGloss(String babelSynsetID)
	{
		return synsetToGloss.get(babelSynsetID);
	}
	
	public double getFEAffinityScore(String babelSynsetID)
	{			
		return synsetToScore.get(babelSynsetID);
	}
	
	public String getFENameBabelSynsetGloss()
	{
		return FENameBabelSynsetGloss;
	}
	
	public int getFEValBabelSynsetListLength()
	{
		return FEValBabelSynsetIDList.size();
	}
	
	public String getFrameElementKey()
	{
		return this.FrameElementKey;
	}
	
	public void setDataType(String dataType)
	{
		this.DataType = dataType;
	}
	
	public boolean checkInteger(String val)
	{
		try { 
	        Integer.parseInt(val); 
	    } catch(NumberFormatException e) { 
	        return false; 
	    } catch(NullPointerException e) {
	        return false;
	    }
		return true;
	}
	
	public void SetProhibitedEdgeList(ArrayList<String> prohibitedEdgeList)
	{
		this.prohibitedEdgeList = prohibitedEdgeList;
	}
	
	public String print()
	{
		String s = "FE: " + FEName + "\n";
		for(int i = 0; i < 3 && i < FEValBabelSynsetIDList.size(); i++)
		{
			String synset = FEValBabelSynsetIDList.get(i);
			if(synsetToScore.get(synset) > 0.0)
			{
				s += "\t" + synset + " " + FEValBabelSynsetGlossList.get(i) + "\n";
				s += "\t\t" + synsetToPath.get(FEValBabelSynsetIDList.get(i)) 
					+ " " + synsetToScore.get(synset) + "\n";
			}
		}
		return s;
	}
}
