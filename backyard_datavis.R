## need to run backyard_glmm.R first

## plot the confidence intervals egg ----
egg.plot <- backyard.egg.confint.df[-c(19:20),] %>% 
  dplyr::mutate(
    color = ifelse(X2.5.. < 0 &X97.5.. > 0, "cross",
    ifelse(X2.5.. < 0 & X97.5.. < 0,
            "negative",
            "positive")
        )
  )

egg.plot$X <- rownames(egg.plot)

my_x_labels <- setNames(
    c(
        "(Intercept)",
        "A. syriaca",
        "A. tuberosa",
        "Height",
        "Predator total - without ants",
        "Ant total",
        "Aphis nerii",
        "Myzocallis asclepiadis",
        "A. syriaca x Height",
        "A. tuberosa x Height",
        "A. syriaca x Predator total - without ants",
        "A. tuberosa x Predator total - without ants",
        "A. syriaca x Ant total",
        "A. tuberosa x Ant total",
        "A. syriaca x Aphis nerii",
        "A. tuberosa x Aphis nerii",
        "A. syriaca x Myzocallis asclepiadis",
        "A. tuberosa x Myzocallis asclepiadis"
    ),
    rownames(egg.plot)
)


egg.plot %>% 
  ggplot(aes(x = X, y=Estimate)) + 
  geom_hline(yintercept = 0, color="red",linetype="dashed") + 
  geom_point(aes(color=color)) +
  geom_errorbar(aes(ymin=X2.5..,ymax=X97.5..,color=color),
                size=1.85,width=0.25) + 
  theme_classic() +
  xlab("Covariate") +
  ylab("Confidence Interval") +
  scale_x_discrete(labels=my_x_labels)+
  theme(
    legend.position = "none",
    text = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(face = "bold", vjust = 3),
    legend.title = element_text(face = "bold"),
    axis.line = element_line(size = 1),
    axis.ticks.x = element_line(size = 1),
    axis.text.x = element_text(angle = 45,vjust=1,hjust=1)
  ) +
  scale_color_manual(name = " ", 
                    values = c("lightgray", "red","black")) +
  ggsave(
    filename = "datavis/egg-results-plot.png",
    dpi = 350
  )



## plot the confidence intervals larvae ---
larv.plot <- backyard.larv.confint.df[-c(16:17),] %>% 
  dplyr::mutate(
    color = ifelse(X2.5.. < 0 &X97.5.. > 0, "cross",
    ifelse(X2.5.. < 0 & X97.5.. < 0,
            "negative",
            "positive")
        )
  )

larv.plot$X <- rownames(larv.plot)

my_x_labels <- setNames(
    c(
      "(Intercept)",
      "Predator total - without ants",
      "Ant total",
      "Aphis nerii",
      "Myzocallis asclepiadis",
      "A. Syriaca",
      "A. Tuberosa",
      "A. syriaca x Predator total - without ants",
      "A. tuberosa x Predator total - without ants",
      "A. syriaca x Ant total",
      "A. tuberosa x Ant total",
      "A. syriaca x Aphis nerii",
      "A. tuberosa x Aphis nerii",
      "A. syriaca x Myzocallis asclepiadis",
      "A. tuberosa x Myzocallis asclepiadis"
    ),
    rownames(larv.plot)
)


larv.plot %>% 
  ggplot(aes(x = X, y=Estimate)) + 
  geom_hline(yintercept = 0, color="red",linetype="dashed") + 
  geom_point(aes(color=color)) +
  geom_errorbar(aes(ymin=X2.5..,ymax=X97.5..,color=color),
                size=1.85,width=0.25) + 
  theme_classic() +
  xlab("Covariate") +
  ylab("Confidence Interval") +
  scale_x_discrete(labels=my_x_labels)+
  theme(
    legend.position = "none",
    text = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(face = "bold", vjust = 3),
    legend.title = element_text(face = "bold"),
    axis.line = element_line(size = 1),
    axis.ticks.x = element_line(size = 1),
    axis.text.x = element_text(angle = 45,vjust=1,hjust=1)
  ) +
  scale_color_manual(name = " ", 
                    values = c("lightgray", "red","black")) +
  ggsave(
    filename = "datavis/larv-results-plot.png",
    dpi = 350
  )
