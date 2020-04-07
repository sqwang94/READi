recpageDIV <- bootstrapPage(
  div(id = "rec_page",
      h4("Consider the following real-world issues before making your recommendation:"),
      hr(),
      p("1. What is the benefit-risk tradeoff?"),
      p("2. Will this decision affect my employee productivity?"),
      p("3. Are important values and preferences of stakeholders (patients/employers/clinicians) considered?"),
      p("4. Is adoption feasible?"),
      p("5. What is the opportunity cost of reviewing RWE?"),
      p("6. Can I afford the intervention?"),
      p("7. What would the patient burden be?"),
      p("8. Will the intervention be acceptable to my providers and patients?"),
      p("9. Can I equitably deliver this intervention across my population?"),
      p("10. What are the sources of the stakeholder information?"),
      hr()
    )
)