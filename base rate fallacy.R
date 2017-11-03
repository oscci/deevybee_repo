#Base rate fallacy
#By DVM Bishop, 3rd Nov 2017, for blogpost on Bishopblog

#Demo with prison data

library(DiagrammeR)
#https://www.gov.uk/government/statistics/prison-population-figures-2017
men.prisoners<-82314
women.prisoners<-4013

#General population figures
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/articles/overviewoftheukpopulation/february2016

population.2014<-64600000
#Excluding those over 80, sex ratio is about 1:1
males.2014<-population.2014/2
#Exclude 19% children
men.2014<-.81*males.2014

#for interest, % population males in prison
p.men.prison<-100*men.prisoners/men.2014

men.free<-men.2014-men.prisoners
#assume rate of DLD in adult men is 5%
DLDrate<-.07
men.DLD<-DLDrate*men.2014
men.noDLD<-(1-DLDrate)*men.2014
#take figure of 50% prisoners have DLD
men.DLD.prison<-.5*men.prisoners
men.noDLD.prison<-men.prisoners-men.DLD.prison
#then percentage of maleDLD who are prisoners is:

p.DLDmen.prison<-100*men.DLD.prison/men.DLD
#then percentage of male nonDLD who are prisoners is:
p.noDLDmen.prison<-100*men.noDLD.prison/men.noDLD
#Now do plot
print(grViz("
digraph prisonplot {
            
            # node definitions with substituted label text
            node [shape = plaintext, fontname = Helvetica]
   
            
            node [shape=square, fixedsize=true, width=1.3,style=filled, color=CornSilk]
            A[label='@@1']
            B[label='@@2']
            
            C[label='@@3']
            D[label='@@4']
            E[label='@@5']
            F[label='@@6']
            G[label='@@7']
  
            
            # edge definitions with the node IDs
            A -> {B C}
            B -> {D E}
            C -> {F G}
    

            }
            
            [1]: paste0('UK men:\\n', ' N = ',as.integer(men.2014/1000))
            [2]: paste0('   In Prison:\\n', ' N = ',as.integer(men.prisoners/1000))
            [3]: paste0('Not in Prison:\\n', 'N = ',as.integer(men.free/1000))
            [4]: paste0('  DLD:\\n', 'N = ',as.integer(men.prisoners*.5/1000))
            [5]: paste0('No DLD:\\n', 'N = ',as.integer(men.prisoners*.5/1000))
            [6]: paste0('  DLD:\\n', 'N = ',as.integer(men.free*DLDrate/1000))
            [7]: paste0('No DLD:\\n', 'N = ',as.integer(men.free*(1-DLDrate)/1000))
   
            "))

