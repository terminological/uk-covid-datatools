ukcovidtools::setup()

test_that("111 data does not crash", {
  dpc$spim$getOneOneOne() %>% attr("paths") %>% print()
  dpc$spim$getOneOneOneLineList(Sys.Date()-7) %>% attr("paths") %>% print()
  dpc$spim$getOneOneOneIncidence(Sys.Date()-7) %>% attr("paths") %>% print()
})


test_that("Deaths data does not crash", {
  dpc$spim$getDeathsLineList() %>% attr("paths") %>% print()
  dpc$spim$getDeathsLineListIncidence(ageBreaks=c(18,25,35,45,55,65,75)) %>% attr("paths") %>% print()
})


test_that("Variants data does not crash", {
  dpc$spim$getVoc351LineList() %>% attr("paths") %>% print()
  dpc$spim$getVAMLineList() %>% attr("paths") %>% print()
})


test_that("CTAS data does not crash", {
  dpc$spim$getCTASLineList() %>% attr("paths") %>% print()
})


test_that("Immunisation data does not crash", {
  dpc$spim$getImmunizationLineList() %>% attr("paths") %>% print()
  #dpc$spim$getImmunizationLineListIincidence()
  dpc$spim$getImmunizationFraction(ageBreaks=c(18,25,35,45,55,65,75)) %>% attr("paths") %>% print()
})

test_that("S-gene data does not crash", {
  dpc$spim$getSGeneLineList() %>% attr("paths") %>% print()
  #dpc$spim$getSDropoutFreqency()
  #dpc$spim$getSGeneEras()
  #dpc$spim$interpretSGene()
})


test_that("Cases data does not crash", {
  dpc$spim$getLineList() %>% attr("paths") %>% print()
  dpc$spim$getLineListIncidence(ageBreaks=c(18,25,35,45,55,65,75)) %>% attr("paths") %>% print()
  #dpc$spim$augmentLineListWithLSOA()
})

test_that("Negatives data does not crash", {
  dpc$spim$getNegatives() %>% attr("paths") %>% print()
})

test_that("Seroprevalence data does not crash", {
  dpc$spim$getSeroprevalence() %>% attr("paths") %>% print()
  dpc$spim$getSeroprevalenceTestIncidence(ageBreaks=c(18,25,35,45,55,65,75))
})

test_that("Chess / SARI data does not crash", {
  dpc$spim$getCHESS() %>% attr("paths") %>% print()
  dpc$spim$getSARI() %>% attr("paths") %>% print()
  dpc$spim$getCHESSSummary() %>% attr("paths") %>% print()
  dpc$spim$getSARISummary() %>% attr("paths") %>% print()
})

test_that("DSTL data does not crash", {
  dpc$spim$getFourNationsCases() %>% attr("paths") %>% print()
  dpc$spim$getSPIMextract() %>% attr("paths") %>% print()
})

