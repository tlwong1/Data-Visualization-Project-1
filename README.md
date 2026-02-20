# Uber Ride Analytics in NCR India — Final Project

## Overview

This project analyzes Uber ride-hailing data from the National Capital Region (NCR) of India to explore questions of platform efficiency, pricing fairness, and driver reliability. The findings are presented as an interactive Quarto RevealJS slideshow.

## Motivation

Ride-hailing services like Uber play a significant role in urban mobility, but pricing transparency and service reliability remain key policy concerns — particularly for short urban trips where base fares and surge pricing have an outsized effect on perceived fairness. This project focuses on two areas:

- **Driver reliability and platform efficiency**
- **Pricing fairness, especially for short urban trips**

## Data Source

- **Dataset:** Uber Ride Analytics Dashboard (sourced from Kaggle)
- **Geography:** National Capital Region (NCR), India
- **Unit of analysis:** Individual ride bookings
- **Key fields:** Booking Status, Booking Value, Ride Distance, Time of Day, Vehicle Type

## Key Visualizations & Findings

**1. Price per Kilometer by Trip Distance**
Fares generally rise with distance, but short trips exhibit much greater pricing dispersion — suggesting that base fare minimums and surge pricing matter more than distance for brief urban rides.

**2. Cancellation Rate by Hour of Day**
Cancellation rates remain relatively stable throughout the day (within ~5 percentage points), with the highest rates occurring in the early morning, likely due to low driver supply.

**3. Peak Usage Hours (Auto)**
Ride demand follows a predictable daily cycle with two peaks — a late-morning surge and a stronger evening peak — enabling proactive driver supply planning.

**4. Weekday vs. Weekend Demand Comparison**
Peak-hour demand patterns are similar across weekdays and weekends, suggesting that operational and policy interventions should be calendar-aware rather than one-size-fits-all.

**5. High-Traffic Routes by Cancellation Rate (Interactive Network Graph)**
An interactive network visualization maps high-traffic routes and highlights those with elevated cancellation rates, helping identify where reliability improvements are most needed.

## How to View the Presentation

Open `final_quarto_presentation.html` in any modern web browser. Use arrow keys or on-screen controls to navigate between slides.

> **Note:** The presentation includes an embedded interactive network graph. Make sure the `final_quarto_presentation_files/` folder is in the same directory as the HTML file for all assets to load correctly.

## Tools & Technologies

- **R** — data wrangling and visualization
- **Quarto** — presentation framework (RevealJS)
- **visNetwork** — interactive network graph
- **ggplot2** — static visualizations
