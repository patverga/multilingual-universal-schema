package edu.umass.cs.iesl

import edu.umass.cs.iesl.tackbp2014.nlp.ChainedNLPComponent
import edu.umass.cs.iesl.tackbp2014.nlp.DeterministicSubstringNerCorefComponent
import edu.umass.cs.iesl.tackbp2014.nlp.SlotFillingLogPatternRelationMentionFindingComponent

/**
 * Created by pv on 3/5/15.
 */


object RelationComponents extends ChainedNLPComponent {
  lazy val components = Seq(
    DeterministicSubstringNerCorefComponent,
    SlotFillingLogPatternRelationMentionFindingComponent
  )
}

object UniversalSchemaPipeline extends App
{
  // Document text
  val exampleDocumentText = "The last time I went to Boston, I visited the home of Paul Revere in Quincy. I also visited the MFA and ate lunch with my friend at Harvard."
  val exampleDoc = EmbeddingEntiyLinking.linkText(exampleDocumentText)
  RelationComponents.process1(exampleDoc)
  println("didnt crash")
}
