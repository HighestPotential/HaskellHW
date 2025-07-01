#vergleich und tauschoperationen

array= [163,128, 120, 115, 123, 94]
print(array)

compare_count=0
swap_count=0

#bubbleSort


# for i, _ in enumerate (array):
#     for j in range (0, len(array)-1-i):
#         compare_count+=1
#         if array[j+1]<array[j]:
#             swap_count+=1
#             array[j], array[j+1]= array[j+1], array[j]


#selectionSort

# for i, _ in enumerate(array):
#     minIndex= i
#     for j in range(i+1, len(array)):
#         compare_count+=1
#         if array[minIndex]>array[j]:
#             minIndex=j
            
#     if minIndex != i:
#         swap_count+=1
#         array[minIndex], array[i]= array[i], array[minIndex] 

#insertionSort

# for i in range(1, len(array)):
#     current=array[i]
#     j=i
#     while(j>0 and array[j-1] > current):
#         compare_count+=1
        
#         array[j]=array[j-1]
#         j-=1
        
#     swap_count+=1
#     array[j]=current
    
    

# print(array)

# print(compare_count, " and ", swap_count)


from scipy.stats import binom

n = 1000
k = 500
p = 0.5

pmf = binom.pmf(k, n, p)
cdf = binom.cdf(k, n, p)
sf  = binom.sf(k, n, p)   # P(X > k) = 1 - CDF(k)

print("P(X = 500):", pmf)
print("P(X <= 500):", cdf)
print("P(X >= 501):", sf)
