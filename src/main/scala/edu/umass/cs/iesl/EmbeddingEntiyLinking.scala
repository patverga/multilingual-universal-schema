package edu.umass.cs.iesl

  import java.io._

  import cc.factorie.la.DenseTensor1
  import edu.umass.cs.iesl.entity_embeddings.data_structures.WikificationStructs.ReferenceProblem
  import edu.umass.cs.iesl.entity_embeddings.data_structures._
  import edu.umass.cs.iesl.entity_embeddings.data_structures.data_stores.{EmbeddingCollection, SurfaceFormDB, TypeDB}
  import edu.umass.cs.iesl.entity_embeddings.embedding.EntityResolver
  import edu.umass.cs.iesl.entity_embeddings.eval.EvalOutput
  import edu.umass.cs.iesl.entity_embeddings.linking.{LogisticRegressionTrainedLinker, NERBasedMentionFinder}
  import edu.umass.cs.iesl.entity_embeddings.util.FileIO
  import edu.umass.cs.iesl.entity_embeddings.{EntityEmbeddingOpts, eval}

  object EmbeddingEntiyLinking extends App {

//    def main(args: Array[String]) = {

      val opts = new EntityEmbeddingOpts()
      opts.parse(args)

      // Read in the language
      val lang = DocLanguage.fromIsoString(opts.language.value)

      // Load the Redirect/Entity Resolution DB
      val entityResolver = EntityResolver.fromCMDOptions(opts, lang)

      // Load the surface form database
      val surfaceFormDB = SurfaceFormDB.fromCMDOptions(opts)

      // Load the typeDB
      val typeDB = TypeDB.fromCMDOptions(opts)

      println("Loading Embeddings")
      // Load the embeddings
      val embeddingCollection = EmbeddingCollection.fromCMDOptions(opts)

    def linkText(inputStr : String, docId : String = "ExampleDocument", lang : Object = English) = {
      // Document Representation for Entity linking
      val elDoc = ELDocument(docId, inputStr,lang=English)

      // Convert to a factorie document
      val fDoc = elDoc.toFactorieDocument

      // Define the mention finder
      val mentionFinder = new NERBasedMentionFinder(surfaceFormDB,English,caseSensitiveMentions = false) // Using case insensitive mentions

      // Define the entity linker
      val features = opts.features.value.map(FeatureType.fromIsoString).toList
      val weights = new DenseTensor1(FileIO.readFeatureWeights(opts.featureWeightsFilename.value.head))
      val contextWindowSize = (opts.window.value, 10000, 10000)
      val entityLinker = new LogisticRegressionTrainedLinker(features, embeddingCollection, surfaceFormDB, typeDB, null, contextWindowSize, opts.caseSensitiveWords.value, opts.caseSensitiveMentions.value, weights = weights)


      println("Running Mention finding")
      // Run the mention finder
      mentionFinder.process(fDoc)

      println("Running Entity Linker")
      // Run the entity Linker
      entityLinker.process(fDoc)
      println()
      println("Final Mention Assignment")
      // Print the results
      for (mention <- fDoc.attr[EntityLinks].mentions) {
        println(mention.mentionSlug + " linked to " + mention.entitySlug + " URL: " + Slug.toWikiURL(mention.entitySlug))
      }

      fDoc
    }
  }

