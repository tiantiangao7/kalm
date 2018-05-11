package main.java.edu.stonybrook.cs.frame;

import java.util.ArrayList;

import main.java.edu.stonybrook.cs.fpparser.FrameDescriptionPredicate;
import main.java.edu.stonybrook.cs.fpparser.PropertyPredicate;

public class LoadFrame {
	public static Frame getFrame(String FrameName)
	{
		return CreateFrame(FrameName);
	}
	
	private static void CompareFrame(Frame f1, Frame f2)
	{
		assert f1.getFrameName().equals(f2.getFrameName());
		assert f1.getAllFrameElements().size() == f2.getAllFrameElements().size();
		ArrayList<FrameElement> l1 = f1.getAllFrameElements();
		ArrayList<FrameElement> l2 = f2.getAllFrameElements();
		for(int i = 0; i < l1.size(); i++)
		{
			assert l1.get(i).getFEName().equals(l2.get(i).getFEName());
			assert l1.get(i).getFENameBabelSynsetID().equals(l2.get(i).getFENameBabelSynsetID());
		}	
	}
	
	public static Frame CreateFrame(String frameName)
	{
		ArrayList<PropertyPredicate> fpList = FrameDescriptionPredicate.GetFramePropertyList(frameName);
		Frame frame = new Frame(frameName);
		for(PropertyPredicate propPredicate : fpList)
		{
			FrameElement fe = new FrameElement(propPredicate.GetPropertyName(),propPredicate.GetBabelNetSID());
			String[] dataTypeList = propPredicate.GetDataTypeConstraint();
			if(dataTypeList != null)
			{
				for(String dataType : dataTypeList)
				{
					if(dataType.equals("Integer"))
					{
						fe.setDataType(dataType);
					}
					else if(dataType.equals("Currency"))
					{
						fe.setDataType(dataType);
					}
				}
			}
			String[] prohibitedEdgeList = propPredicate.GetProhibitedEdgeList();
			if(prohibitedEdgeList != null)
			{
				ArrayList<String> list = new ArrayList<String>();
				for(String edge : prohibitedEdgeList)
				{
					list.add(edge);
				}
				fe.SetProhibitedEdgeList(list);
			}
			frame.addFrameElement(fe);
		}
		return frame;
	}
	
	public static void test(String frameName)
	{
		Frame f1 = CreateFrame(frameName);
		Frame f2 = getFrame(frameName);
		CompareFrame(f1,f2);
	}
	
	public static void main(String[] args) {
		FrameDescriptionPredicate.Parse();
		test("Being_employed");
		test("Age");
		test("Human_gender");
		test("Residence");
		test("Being_born");
		test("Renting");
		test("Commerce_buy");
		test("Possession");
		test("Cooking");
		test("Dressing");
		test("Create_Organization");
		test("Travel");
		test("Education");
		test("Giving");
		test("Personal_relationship");
		test("People_by_origin");
		test("People_by_religion");
		test("Collaboration");
		test("Quitting");
		test("Medical_conditions");
		test("Award");
	}
}
