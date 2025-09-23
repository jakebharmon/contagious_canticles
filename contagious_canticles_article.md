# Contagious Canticles

*Remember, Lord, our mortal state;  
How frail our lives! how short the date!  
Where is the man that draws his breath,  
Safe from disease, secure from death?*

**547 Granville**

Many things set Sacred Harp music apart. There is of course the strange
*shape* of it all, from notes to book to seats; there is near-Athenian
style democracy of leading lessons; there is the fact that this niche
tradition has become a global phenomenon; and, there are the minutes.
This long standing tradition of recording and publishing the leaders and
songs dates back at least to the early 1900’s, when the minutes would be
printed in local newspapers, or later in pamphlets and bound books.

I began singing Sacred Harp in the middle of my graduate studies of
Epidemiology, and when I was first introduced to the FaSoLa minutes app
I was taken aback at the wealth of information. My head automatically
began to see the way this data rhymed with my case studies at school,
where we tracked back exposures and of infectious disease through
contact tracing.

In the middle of these thoughts, I heard for the first time 268, or
David’s Lamentation. In this song, the common fugue uniquely struck me,
as I heard the booming basses as none other than the aged King David
breaking out in tears as in Psalm 51. I choose this song for my lesson
many times afterwards. And, over time, I formulated in my head a
research question: what if one thought of songs as infectious, or dare I
even say viral?

Infamously, it is up to interpretation whether viruses are living or
dead; they are parcels of RNA - the building block of life - that can
only replicate themselves in a host cell. Songs, for their part, are not
all so different; we encode their DNA with lyrics and notes, and yet
they exist as mere ink on the pages until they burst forth from our
lips.

I decided, therefore, to apply my epidemiology tools to the Sacred Harp
Minutes. The methodology is imperfect, as minutes only list those who
lead lessons, but, given, that it is common for most in attendance to
lead at least one song, I wagered this could be a good stand-in for
those in attendance.

In doing this, I was able to build long lists for each individual
person, recording the very first time they were “exposed” to a song,
whether they ever led it, and - if so - how long it took them to lead
it. I average out this time-to-lead by each song, as if considering each
to be its own virus, and conducted what is called a Survival Analysis,
looking at how long it takes someone to lead a song within a year of
their first exposure to it. I compiled all the data and created a web
app, where you can look at a song either in isolation or beside others:

https://51t3yt-jake-harmon.shinyapps.io/sacred_harp_km_app/

This app lets you pick from around 500 songs, and it lets you even
select the range of time, assuming that songs may be more-or-less
contagious based on the time of year and the culture/attitudes prevalent
then. This, of course, is an overwhelming amount of information, so I
asked a simple exploratory question: is there a difference in
“contagiousness” between the songs that were kept in the new 2025
edition versus those removed? This required running a special
statistical test called a Log Rank Test, which is used specifically for
comparing groups in Survival Analysis.

    Call:
    survdiff(formula = Surv(time_to_event_days, event_status) ~ removed, 
        data = survival_df)

                  N Observed Expected (O-E)^2/E (O-E)^2/V
    removed=0 19947    19947    19953   0.00166    0.0718
    removed=1   538      538      532   0.06217    0.0718

     Chisq= 0.1  on 1 degrees of freedom, p= 0.8 

Above is the statistical output of my Log Rank test for the songs kept
versus those removed. In statistics, a P Value is a short hand for
describing whether a difference was due to random chance or not;
scientists often select p = 0.05 to mean that it was *unlikely* an event
occurred by pure chance. The P value here being 0.8 means that, based on
the minutes, there is not a great difference in the “contagiousness” of
the songs which were kept versus those removed.

There are many more ways to look at this data, and I plan to keep
exploring it, while also encouraging others to take their own peaks at
it. But, at the end of this little adventure, I can only think of the
complexity of our tastes. Each time we lead a lesson, we are surely
aware of many reasons why we are choosing that song - whether it be in
memory of a friend, to celebrate a holiday, or simply because we haven’t
heard it in a while. There are surely many reasons too of which we are
unaware, and yet our subconscious inextricably draws us towards it. And
yet, I am personally convinced, that there is still yet a third set of
reasons, which are completely unknowable, for “the wind bloweth where it
listeth.”
